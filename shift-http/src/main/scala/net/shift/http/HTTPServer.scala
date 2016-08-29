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
import akka.actor.PoisonPill
import net.shift.io.Cont
import net.shift.io.Data
import net.shift.io._
import net.shift.io.EOF
import net.shift.io.Done

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

  srv.start(serve)
}

object HTTPServer {

  def apply() = new HTTPServer(ServerSpecs("Shift-HTTPServer", "0.0.0.0", 8080))

  def apply(name: String) = new HTTPServer(ServerSpecs(name, "0.0.0.0", 8080))

  def apply(port: Int) = new HTTPServer(ServerSpecs("Shift-HTTPServer", "0.0.0.0", port))
}

case class HTTPServer(specs: ServerSpecs) extends Log {

  protected[http] val system = ActorSystem("HTTPServer")

  private lazy val serverActor = system.actorOf(Props[ServerActor])

  private val selector = Selector.open

  def loggerName = specs.name

  @volatile
  private var running = false;

  def start(service: HTTPService): Future[Unit] = {

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    val address = new InetSocketAddress(specs.address, specs.port)
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
            key.channel().close()
            key.cancel()
          } else {
            if (key.isValid()) {
              if (key.isAcceptable()) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)
                  log.info("Accepted new connection from " + client.getRemoteAddress)

                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  serverActor ! ClientConnect(clientKey)
                }
              } else if (key.isReadable()) {
                serverActor ! ReadHttp(key, service)
              } else if (key.isWritable()) {
                serverActor ! WriteHttp(key)
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

class ServerActor extends Actor with ActorLogging {
  private val actors = new TrieMap[SelectionKey, ActorRef]

  def receive = {
    case ClientConnect(key) =>
      val clientActor = context.actorOf(Props[ClientActor])
      actors.put(key, clientActor)
    case ClientTerminate(key) =>
      log.info("Client terminated")
      key.cancel()
      actors -= key
      sender ! PoisonPill
      key.channel().asInstanceOf[SocketChannel].close()

    case r: ReadHttp  => actors.get(r.key) map (_ ! r)
    case w: WriteHttp => actors.get(w.key) map (_ ! w)

  }
}

trait ServerMessage

case class ReadHttp(key: SelectionKey, service: HTTPService) extends ServerMessage
case class WriteHttp(key: SelectionKey) extends ServerMessage
case class ClientTerminate(key: SelectionKey) extends ServerMessage
case class ClientConnect(key: SelectionKey) extends ServerMessage

class ClientActor() extends Actor with ActorLogging {

  var readState: Option[Payload] = None
  var writeState: Option[ByteBuffer] = None

  var keepAlive: Boolean = false

  val responseIteratee: Iteratee[ByteBuffer, Unit] = Cont {
    case Data(d) => responseIteratee
    case EOF     => Done((), EOF)
  }

  def receive = {
    case r: ReadHttp  => readChunk(r)
    case w: WriteHttp => writeResponse(w)
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): ByteBuffer = {
    var written = client.write(buffer)
    while (written > 0 && buffer.hasRemaining()) {
      written = client.write(buffer)
    }
    buffer
  }

  private def handleResponseSent(key: SelectionKey) = {
    writeState = None
    if (!keepAlive) {
      sender ! ClientTerminate(key)
    } else {
      key.interestOps(SelectionKey.OP_READ)
    }
  }

  private def writeResponse(wr: WriteHttp) = {
    writeState match {
      case Some(resp) =>
        val client = wr.key.channel().asInstanceOf[SocketChannel]
        val buf = drain(client, resp)

        if (!resp.hasRemaining()) {
          handleResponseSent(wr.key)
        }
      case _ =>
    }

  }

  private def readChunk(r: ReadHttp) = {

    def checkRequestComplete(bodySize: Long, contentLength: Option[Long], http: HTTPRequest) = {
      if (contentLength.getOrElse(-1L) > bodySize) {
        readState = Some(http)
      } else {
        readState = None
        keepAlive = http.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
        r.service(http, resp => {
          IO.toBuffer(resp) map { arr =>
            val client = r.key.channel().asInstanceOf[SocketChannel]
            val buf = drain(client, arr)

            if (buf.hasRemaining()) {
              writeState = Some(buf)
              r.key.interestOps(SelectionKey.OP_WRITE)
              r.key.selector().wakeup()
            } else {
              handleResponseSent(r.key)
            }
          }
        })
      }
    }

    val client = r.key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)

    if (size > 0) {
      buf.flip()
      readState match {
        case RawExtract(raw) =>
          val msg = raw + buf
          tryParse(msg) match {
            case Some(http) =>
              checkRequestComplete(http.body.size, http.longHeader("Content-Length"), http)
            case None =>
              readState = Some(msg)
          }
        case Some(h @ HTTPRequest(m, u, v, hd, body)) =>
          val newSize = body.size + size
          val cl = h.longHeader("Content-Length")
          val msg = HTTPBody(body.parts ++ Seq(buf))
          val req = HTTPRequest(m, u, v, hd, msg)
          checkRequestComplete(newSize, cl, req)
      }

    } else if (size < 0) {
      readState = None
      sender ! ClientTerminate(r.key)
    }
  }

  private def tryParse(msg: Raw): Option[HTTPRequest] = {
    BinReader(IO.fromChunks(msg.buffers)).toOption.flatMap { reader =>
      new HttpParser().parse(reader) match {
        case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
          Some(h)
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

case class ServerSpecs(name: String,
                       address: String,
                       port: Int)
