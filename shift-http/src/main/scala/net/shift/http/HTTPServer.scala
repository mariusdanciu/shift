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
import akka.actor.PoisonPill
import scala.collection.concurrent.TrieMap
import net.shift.common.BinReader
import scala.util.Failure
import scala.util.Success
import java.io.IOException

object Test extends App {
  def serve: HTTPService = req => resp => {
    println(req)
    resp(Responses.textResponse("Some response"))
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

  def apply() = new HTTPServer(ServerSpecs())

  def apply(serverName: String) = new HTTPServer(ServerSpecs(name = serverName))

  def apply(serverPort: Int) = new HTTPServer(ServerSpecs(port = serverPort))
}

case class HTTPServer(specs: ServerSpecs) extends Log {

  protected[http] val system = ActorSystem("HTTPServer")

  //private lazy val serverActor = system.actorOf(Props[ServerActor])

  private val selector = Selector.open

  private[http] val clients = new TrieMap[SelectionKey, ClientHandler]

  def loggerName = specs.name

  @volatile
  private var running = false;

  def start(service: HTTPService)(implicit ctx: ExecutionContext): Future[Unit] = {

    @tailrec
    def loop(serverChannel: ServerSocketChannel) {
      if (running) {

        val r = selector.select()

        val keys = selector.selectedKeys().iterator()

        while (keys.hasNext()) {
          val key = keys.next()
          keys.remove()

          if (!running) {
            closeClient(key)
          } else {
            if (key.isValid()) {
              if (key.isAcceptable()) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)
                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  val clientName = client.getRemoteAddress.toString + "-" + key
                  clients.put(clientKey, new ClientHandler(clientKey, clientName, k => {
                    closeClient(k)
                    clients remove k
                  }))
                }
              } else if (key.isReadable()) {
                clients get (key) map { _.readChunk(service) }
              } else if (key.isWritable()) {
                unSelectForWrite(key)
                clients get (key) map { _.writeResponse() }
              }
            }
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
        //system.stop(serverActor)
        system.shutdown()
    }
  }

  private def closeClient(key: SelectionKey) {
    key.channel().close()
    key.cancel()
  }

  def stop() = {
    running = false;
    selector.wakeup()
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

case class ServerSpecs(name: String = "Shift-HTTPServer",
                       address: String = "0.0.0.0",
                       maxParallelConnections: Int = -1,
                       port: Int = 8080)


