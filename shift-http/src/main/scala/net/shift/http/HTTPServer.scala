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
import java.util.logging.Logging
import java.util.logging.Logging
import java.util.logging.Logging
import akka.event.Logging
import akka.actor.ActorLogging

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
  val state = ServerState(new TrieMap[SelectionKey, ActorRef], new TrieMap[SelectionKey, Payload])

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
            state.payloads -= key
            key.channel().close()
            key.cancel()
          } else {
            if (key.isValid()) {
              if (key.isAcceptable()) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)
                  log.info("Accepter new connection from " + client.getRemoteAddress)

                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  state.actors.put(clientKey, system.actorOf(Props[ClientActor]))
                }
              } else if (key.isReadable()) {
                state.actors(key) ! Read(key, state, service)
              } else if (key.isWritable()) {
                state.actors(key) ! Write(key, state)
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

private[http] case class ServerState(
  actors: TrieMap[SelectionKey, ActorRef],
  payloads: TrieMap[SelectionKey, Payload])

trait ServerMessage

case class Read(key: SelectionKey, state: ServerState, service: HTTPService) extends ServerMessage
case class Write(key: SelectionKey, state: ServerState) extends ServerMessage

class ClientActor extends Actor with ActorLogging {

  def receive = {
    case r: Read  => readChunk(r)
    case w: Write => writeResponse(w)
  }

  private def writeResponse(wr: Write) = {
    (wr.state.payloads get wr.key) match {
      case Some(Raw(resp :: Nil)) =>
        val client = wr.key.channel().asInstanceOf[SocketChannel]
        val written = client.write(resp)
        if (!resp.hasRemaining()) {
          wr.state.payloads -= wr.key
          wr.key.cancel()
          client.close()
          context.stop(self)
        }
      case _ => log.error("Cannot handle response")
    }

  }

  private def readChunk(r: Read) = {

    def checkRequestComplete(bodySize: Long, contentLength: Long, http: HTTPRequest) = {
      if (contentLength > bodySize) {
        r.state.payloads += (r.key -> http)
      } else {
        r.state.payloads -= r.key
        r.service(http, resp => {
          IO.toBuffer(resp) map { arr =>
            r.state.payloads += (r.key -> Raw(List(arr)))
            r.key.interestOps(SelectionKey.OP_WRITE)
            r.key.selector().wakeup()
          }
        })
      }
    }

    val client = r.key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)

    if (size > 0) {
      buf.flip()
      (r.state.payloads get r.key) match {
        case RawExtract(raw) =>
          val msg = raw + buf
          tryParse(msg) match {
            case Some(http) =>
              checkRequestComplete(http.body.size, http.contentLength, http)
            case None =>
              r.state.payloads += (r.key -> msg)
          }
        case Some(h @ HTTPRequest(m, u, v, hd, body)) =>
          val newSize = body.size + size
          val cl = h.contentLength
          val msg = HTTPBody(body.parts ++ Seq(buf))
          val req = HTTPRequest(m, u, v, hd, msg)
          checkRequestComplete(newSize, cl, req)
      }

    } else if (size < 0) {
      r.state.payloads -= r.key
      r.key.cancel()
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

  def unapply(t: Option[Payload]): Option[Raw] = t match {
    case None           => Some(Raw(Nil))
    case Some(raw: Raw) => Some(raw)
    case _              => None
  }
}

case class Raw(buffers: List[ByteBuffer]) extends Payload {
  def +(b: ByteBuffer) = Raw(buffers ++ List(b))
  def ++(b: Seq[ByteBuffer]) = Raw(buffers ++ b)
}

