package net.shift.http

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.nio.channels.SocketChannel
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import net.shift.common.BinReader
import net.shift.io.IO
import java.util.concurrent.Executors
import net.shift.common.Log
import org.apache.log4j.BasicConfigurator

object Test extends App {
  def serve: HTTPService = {
    case (req, f) =>
      println(req)
      println(IO.toString(req.body))
      f(Responses.text("Some response"))
  }

  BasicConfigurator.configure

  val srv = HTTPServer()

  import scala.concurrent.ExecutionContext.Implicits.global
  Future {
    //Thread.sleep(10000)
    //srv.stop
  }

  srv.start(serve, 8080)
}

object HTTPServer {

  def apply() = new HTTPServer("Shift-HTTPServer")

  def apply(name: String) = new HTTPServer(name)
}

class HTTPServer(name: String) extends Log {

  protected[http] val system = ActorSystem("HTTPServer")
  val selector = Selector.open

  def loggerName = name

  @volatile
  private var running = false;
  val actors: TrieMap[SelectionKey, ActorRef] = new TrieMap[SelectionKey, ActorRef]

  def start(service: HTTPService, port: Int): Future[Unit] = {
    start(service, "*", port)
  }

  def start(service: HTTPService, interface: String, port: Int): Future[Unit] = {

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    val address = new InetSocketAddress(port)
    serverChannel.bind(address)
    log.info("Server bound to " + address)

    serverChannel.register(selector, SelectionKey.OP_ACCEPT, null)

    running = true

    implicit val ctx = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    Future {
      while (running) {

        val r = selector.select()

        val keys = selector.selectedKeys().iterator()

        while (keys.hasNext()) {
          val key = keys.next()
          if (!running) {
            key.attach(null)
            key.cancel()
          } else {
            if (key.isValid()) {
              if (key.isAcceptable()) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)
                  log.info("Accepter new connection from " + client.getRemoteAddress)

                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  actors.put(clientKey, system.actorOf(Props[ClientActor]))
                }
              } else if (key.isReadable()) {
                actors(key) ! Read(key, service)
              } else if (key.isWritable()) {
                actors(key) ! Write(key)
              }
            }
            keys.remove()
          }
        }
      }
      log.info("Shutting down server")
      serverChannel.close()
      system.shutdown()
      ctx.shutdown()
    }
  }

  def stop() = {
    running = false;
    selector.wakeup()
  }

}

trait ServerMessage

case class Read(key: SelectionKey, service: HTTPService) extends ServerMessage
case class Write(key: SelectionKey) extends ServerMessage

class ClientActor extends Actor {

  def receive = {
    case Read(key, service) => readChunk(key, service)
    case Write(key)         => writeResponse(key)
  }

  private def writeResponse(key: SelectionKey) = {
    val resp = key.attachment().asInstanceOf[ByteBuffer]
    if (resp != null) {
      val client = key.channel().asInstanceOf[SocketChannel]
      val written = client.write(resp)
      if (!resp.hasRemaining()) {
        key.attach(null)
        key.cancel()
        client.close()
        context.stop(self)
      }
    }
  }

  private def readChunk(key: SelectionKey, service: HTTPService) = {

    def checkRequestComplete(bodySize: Long, contentLength: Long, http: HTTPRequest, key: SelectionKey) = {
      if (contentLength > bodySize) {
        key.attach(http)
      } else {
        key.attach(null)
        service(http, resp => {
          IO.toBuffer(resp) map { arr =>
            key.attach(arr)
            key.interestOps(SelectionKey.OP_WRITE)
            key.selector().wakeup()
          }
        })
      }
    }

    val client = key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)

    if (size > 0) {
      buf.flip()
      key.attachment() match {
        case RawExtract(bufs) =>
          val msgs = bufs ++ List(buf)

          tryParse(msgs) match {
            case Some(http) =>
              val sz = http.body.size
              val cl = http.contentLength
              checkRequestComplete(sz, cl, http, key)
            case None =>
              key.attach(msgs)
          }
        case h @ HTTPRequest(m, u, v, hd, body) =>
          val newSize = body.size + size
          val cl = h.contentLength
          val msg = HTTPBody(body.parts ++ Seq(buf))
          val req = HTTPRequest(m, u, v, hd, msg)
          checkRequestComplete(newSize, cl, req, key)

      }
    } else if (size < 0) {
      key.attach(null)
      key.cancel()
      client.close()
      context.stop(self)
    }
  }

  private def tryParse(msg: Raw): Option[HTTPRequest] = {
    BinReader(IO.fromChunks(msg.buffers)).toOption.flatMap { reader =>
      new HttpParser().parse(reader) match {
        case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
          for { cl <- h.header("Content-Length") } yield {
            h
          }
        case Failure(f) =>
          None
      }
    }
  }

}

object RawExtract {

  def unapply(t: Any): Option[Raw] =
    if (t == null)
      Some(Raw(Nil))
    else {
      t match {
        case r: Raw => Some(Raw(r.buffers))
        case _      => None
      }
    }
}

case class Raw(buffers: List[ByteBuffer]) {
  def +(b: ByteBuffer) = Raw(buffers ++ List(b))
  def ++(b: Seq[ByteBuffer]) = Raw(buffers ++ b)
}

