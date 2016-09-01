package net.shift.http

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.nio.channels.SocketChannel
import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

import org.apache.log4j.BasicConfigurator

import Selections._
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import net.shift.common.Log
import net.shift.io._
import net.shift.io.IO

object Test extends App {
  def serve: HTTPService = {
    case (req, f) =>
      println(req)
      println(IO.producerToString(req.body))
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

  implicit val ctx = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  def loggerName = specs.name

  @volatile
  private var running = false;

  def start(service: HTTPService): Future[Unit] = {

    @tailrec
    def loop(serverChannel: ServerSocketChannel) {
      if (running) {

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
                unSelectForWrite(key)
                serverActor ! WriteHttp(key)
              }
            }
            keys.remove()
          }
        }
        loop(serverChannel)
      }
    }

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    val address = new InetSocketAddress(specs.address, specs.port)
    serverChannel.bind(address)
    log.info("Server bound to " + address)

    serverChannel.register(selector, SelectionKey.OP_ACCEPT, null)

    running = true

    val listen = Future {
      loop(serverChannel)
    }

    listen.map {
      case _ =>
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

case class ReadHttp(key: SelectionKey, service: HTTPService) extends ServerMessage
case class WriteHttp(key: SelectionKey) extends ServerMessage
case class ClientTerminate(key: SelectionKey) extends ServerMessage
case class ClientConnect(key: SelectionKey) extends ServerMessage

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
