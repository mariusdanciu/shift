package net.shift
package netty

import java.net.InetSocketAddress
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.buffer.ChannelBufferInputStream
import org.jboss.netty.buffer.ChannelBufferOutputStream
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.channel.ChannelFuture
import org.jboss.netty.channel.ChannelFutureListener
import org.jboss.netty.channel.ChannelHandlerContext
import org.jboss.netty.channel.ChannelPipeline
import org.jboss.netty.channel.ChannelPipelineFactory
import org.jboss.netty.channel.Channels._
import org.jboss.netty.channel.ExceptionEvent
import org.jboss.netty.channel.MessageEvent
import org.jboss.netty.channel.SimpleChannelUpstreamHandler
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.handler.codec.http.{ Cookie => NettyCookie }
import org.jboss.netty.handler.codec.http.CookieDecoder
import org.jboss.netty.handler.codec.http.CookieEncoder
import org.jboss.netty.handler.codec.http.DefaultCookie
import org.jboss.netty.handler.codec.http.DefaultHttpResponse
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpRequestDecoder
import org.jboss.netty.handler.codec.http.HttpResponseEncoder
import org.jboss.netty.handler.codec.http.HttpResponseStatus
import org.jboss.netty.handler.codec.http.HttpVersion._
import org.jboss.netty.handler.codec.http.QueryStringDecoder
import common._
import engine.Engine
import engine.ShiftApplication
import engine.http.Cookie
import engine.http.Request
import engine.http.Response
import net.shift.common.EmptyPath
import net.shift.common.EmptyPath
import net.shift.common.EmptyPath
import net.shift.common.Path
import net.shift.common.Path
import net.shift.common.Path
import net.shift.loc.Language
import scalax.io._
import net.shift.common.PathUtils

object NettyServer {

  def start(port: Int, app: ShiftApplication)(implicit ec: scala.concurrent.ExecutionContext) {
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

private[netty] class HttpServerPipelineFactory(app: ShiftApplication)(implicit ec: scala.concurrent.ExecutionContext) extends ChannelPipelineFactory {

  def getPipeline(): ChannelPipeline = {
    val pipe = pipeline();
    pipe.addLast("decoder", new HttpRequestDecoder());
    pipe.addLast("encoder", new HttpResponseEncoder());
    pipe.addLast("handler", new HttpRequestHandler(app));

    pipe
  }
}

private[netty] class HttpRequestHandler(app: ShiftApplication)(implicit ec: scala.concurrent.ExecutionContext) extends SimpleChannelUpstreamHandler with StringUtils with PathUtils {
  import scala.collection.JavaConversions._
  import NettyHttpExtractor._

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request = e.getMessage().asInstanceOf[HttpRequest]
    val uriStr = request.getUri()
    val queryStringDecoder = new QueryStringDecoder(uriStr);
    val cookieDecoder = new CookieDecoder();
    val httpMethod = request.getMethod().getName();
    val httpParams = parameters(queryStringDecoder)
    val heads = headers(request)
    val cookiesSet = heads.get("Cookie").map(c => asScalaSet(cookieDecoder.decode(c)));
    val qs = queryString(uriStr)

    val buffer = new ChannelBufferInputStream(request.getContent())

    val shiftRequest = new Request {
      lazy val path = Path(uriPath(uriStr))
      def uri = uriStr
      def method = httpMethod
      def contextPath: Path = EmptyPath
      lazy val queryString = qs
      def param(name: String) = params.get(name)
      def params = httpParams
      def header(name: String) = heads.get(name)
      def headers = heads
      lazy val contentLength = header("Content-Length").map(toLong(_, 0))
      def contentType = header("Content-Type")
      lazy val cookies = cookiesMap(cookiesSet)
      def cookie(name: String) = cookies.get(name)
      lazy val readBody = Resource.fromInputStream(buffer)
      def resource(path: Path) = fromPath(path)
      lazy val language = Language("en")
    }

    Engine.run(app)(shiftRequest, writeResponse(request, e))

  }

  private def writeResponse(r: HttpRequest, e: MessageEvent)(resp: Response) {
    val keepAlive = isKeepAlive(r);
    val response = new DefaultHttpResponse(HTTP_1_1, new HttpResponseStatus(resp.code, resp.reason));
    val buf = ChannelBuffers.dynamicBuffer(32768)
    val out = new ChannelBufferOutputStream(buf)

    resp.writeBody(Resource.fromOutputStream(out))
    response.setContent(buf)

    resp.contentType.map(c => response.setHeader("Content-Type", c))

    response.setHeader("Content-Length", response.getContent().readableBytes())

    val cookieEncoder = new CookieEncoder(true);
    if (!resp.cookies.isEmpty) {
      for (sc <- resp.cookies) {
        cookieEncoder.addCookie(new DefaultCookie(sc.name, sc.value) {
          override def getDomain(): String = sc.domain getOrElse null
          override def getPath(): String = sc.path getOrElse null
          override def getMaxAge(): Int = (sc.maxAge getOrElse 0L).toInt
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
