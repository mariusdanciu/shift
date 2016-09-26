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
import net.shift.common.Config
import java.util.concurrent.ExecutorService
import akka.dispatch.ExecutorServiceFactory
import akka.dispatch.ExecutorServiceFactory

object HTTPServer {
  def apply() = new HTTPServer(ServerSpecs())
}

case class HTTPServer(specs: ServerSpecs) extends Log {

  protected[http] val system = ActorSystem("HTTPServer")

  private val selector = Selector.open

  private[http] val clients = new TrieMap[SelectionKey, ClientHandler]

  def loggerName = specs.name

  @volatile
  private var running = false;

  def start(service: HTTPService): Future[Unit] = {

    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(specs.numThreads))

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

  def buffersState: String = buffers map { b => s"${b.position} : ${b.limit}" } mkString "\n"
}

object ServerSpecs {
  def apply() = fromConfig(new Config())

  def fromConfig(conf: Config): ServerSpecs = {
    ServerSpecs(
      name = conf.string("http.serverName", "Shift-HTTPServer"),
      address = conf.string("http.bindAddress", "0.0.0.0"),
      port = conf.int("http.port", 8080),
      numThreads = conf.int("http.numThreads", Runtime.getRuntime.availableProcessors()))
  }
}
case class ServerSpecs(name: String,
                       address: String,
                       port: Int,
                       numThreads: Int)


