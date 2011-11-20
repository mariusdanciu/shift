package net.shift
package netty

import java.io.InputStream
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import common.StringUtils._  
import engine.ShiftApplication
import engine.http.{ReadChannel, WriteChannel, Request, Response, Cookie}

import org.jboss.netty.channel.Channels._;
import org.jboss.netty.handler.codec.http.HttpHeaders._;
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._;
import org.jboss.netty.handler.codec.http.HttpResponseStatus._;
import org.jboss.netty.handler.codec.http.HttpVersion._;


import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers, 
  ChannelBufferInputStream, ChannelBufferOutputStream};


import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelFutureListener;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

import org.jboss.netty.handler.codec.http.{Cookie => NettyCookie, DefaultCookie};
import org.jboss.netty.handler.codec.http.CookieDecoder;
import org.jboss.netty.handler.codec.http.CookieEncoder;
import org.jboss.netty.handler.codec.http.DefaultHttpResponse;
import org.jboss.netty.handler.codec.http.HttpChunk;
import org.jboss.netty.handler.codec.http.HttpChunkTrailer;
import org.jboss.netty.handler.codec.http.HttpContentCompressor;
import org.jboss.netty.handler.codec.http.HttpRequest;
import org.jboss.netty.handler.codec.http.HttpRequestDecoder;
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpResponseStatus};
import org.jboss.netty.handler.codec.http.HttpResponseEncoder;
import org.jboss.netty.handler.codec.http.QueryStringDecoder;

import org.jboss.netty.util.CharsetUtil;

import scala.collection.immutable.TreeMap

object NettyServer {

  def start(port: Int, app: ShiftApplication) {
    val bootstrap = new ServerBootstrap(
      new NioServerSocketChannelFactory(
	Executors.newCachedThreadPool(),
	Executors.newCachedThreadPool()));
    
    // Set up the event pipeline factory.
    bootstrap.setPipelineFactory(new HttpServerPipelineFactory(app));
    
    // Bind and start to accept incoming connections.
    bootstrap.bind(new InetSocketAddress(port));

  }

}

private[netty] class HttpServerPipelineFactory(app: ShiftApplication) extends ChannelPipelineFactory {

  def getPipeline(): ChannelPipeline = {
    val pipe = pipeline();
    pipe.addLast("decoder", new HttpRequestDecoder());
    pipe.addLast("encoder", new HttpResponseEncoder());
    pipe.addLast("handler", new HttpRequestHandler(app));
    
    pipe;
  }
}

private[netty] class HttpRequestHandler(app: ShiftApplication) extends SimpleChannelUpstreamHandler {
  import scala.collection.JavaConversions._
  import NettyHttpExtractor._
  
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent)  {
    val request = e.getMessage().asInstanceOf[HttpRequest]
    val uri = request.getUri()
    val queryStringDecoder = new QueryStringDecoder(uri);
    val cookieDecoder = new CookieDecoder();
    val method = request.getMethod().getName();
    val params = parameters(queryStringDecoder)
    val heads = headers(request)
    val cookiesSet = heads.get("Cookie").map(c => asScalaSet(cookieDecoder.decode(c)));
    val qs = queryString(uri)

    val buffer = new ChannelBufferInputStream(request.getContent())
    val readChannel = new ReadChannel {
      def readBuffer(buf: Array[Byte]): Int = buffer.read(buf)
      def readInt : Int = buffer.read()
      def readByte: Byte = buffer.readByte()
      def readLong: Long = buffer.readLong()
      def readFloat: Float = buffer.readFloat()
      def readDouble: Double = buffer.readDouble()
      def readShort: Short = buffer.readShort()
      def readBoolean: Boolean = buffer.readBoolean()
      def readChar: Char = buffer.readChar()
   }

    val shiftRequest = new Request {
      def path: String = uri
      def method : String = method
      def contextPath : String = ""
      def queryString: Option[String] = qs
      def param(name: String): List[String] = params.get(name).getOrElse(Nil)
      def params: Map[String, List[String]] = params
      def header(name: String): Option[String] = heads.get(name)
      def headers: Map[String, String] = heads
      lazy val contentLength: Option[Long] = header("Content-Length").map(toLong(_, 0))
      def contentType: Option[String] = header("Content-Type")
      lazy val cookies: Map[String, Cookie] = cookiesMap(cookiesSet)
      def cookie(name: String): Option[Cookie] = cookies.get(name)
      def readBody: ReadChannel = readChannel
    }

    for (handle <- app.routes.map(r => r(shiftRequest)).find(!_.isEmpty); 
         f <- handle) {
      f(writeResponse(request, e))
    }

  } 


  private def writeResponse(r: HttpRequest, e: MessageEvent)(resp: Response) {
    val keepAlive = isKeepAlive(r);
    val response = new DefaultHttpResponse(HTTP_1_1, new HttpResponseStatus(resp.code, resp.reason));
    val buf = ChannelBuffers.dynamicBuffer(32768)
    val out = new ChannelBufferOutputStream(buf)
   
    resp.writeBody(new WriteChannel {
      def writeBuffer(v: Array[Byte]) = out.write(v)
      def writeInt(v: Int) = out.write(v)
      def writeByte(v: Byte) = out.writeByte(v)
      def writeLong(v: Long) = out.writeLong(v)
      def writeFloat(v: Float) = out.writeFloat(v)
      def writeDouble(v: Double) = out.writeDouble(v)
      def writeShort(v: Short) = out.writeShort(v)
      def writeBoolean(v: Boolean) = out.writeBoolean(v)
      def writeString(v: String) = out.writeChars(v)
      def writeUTF8(v: String) = out.writeUTF(v)
      def writeChar(v: Char) = out.writeChar(v.toInt)
      def bytesWritten: Long = out.writtenBytes
   })
   response.setContent(buf)

   resp.contentType.map(c => response.setHeader("Content-Type", c))

   response.setHeader("Content-Length", response.getContent().readableBytes())
     
   val cookieEncoder = new CookieEncoder(true);
   if (!resp.cookies.isEmpty){
     for (sc <- resp.cookies) {
       cookieEncoder.addCookie(new DefaultCookie(sc.name, sc.value) {
         override def getDomain(): String = sc.domain getOrElse null
         override def getPath(): String = sc.path getOrElse null
         override def getMaxAge(): Int = sc.maxAge getOrElse 0
         override def getVersion(): Int = sc.version getOrElse 0
         override def isSecure(): Boolean = sc.secure
         override def isHttpOnly(): Boolean = sc.httpOnly
       })
     }
     response.setHeader("Set-Cookie", cookieEncoder.encode());
   }

   val future = e.getChannel().write(response);
 
   if (!keepAlive) {
     future.addListener(ChannelFutureListener.CLOSE);
   }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    e.getCause().printStackTrace();
    e.getChannel().close();
  }
}
