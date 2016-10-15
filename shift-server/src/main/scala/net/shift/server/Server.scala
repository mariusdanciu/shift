package net.shift.server

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import Selections._
import akka.actor.ActorSystem
import net.shift.common.Log
import net.shift.io._
import scala.collection.concurrent.TrieMap
import net.shift.common.Config
import net.shift.protocol.Protocol
import net.shift.http.Payload

object Server {
  def apply() = new Server(ServerSpecs())
}

case class Server(specs: ServerSpecs) extends Log {

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, ClientHandler]

  def loggerName = specs.name

  @volatile
  private var running = false;

  def start(protocol: Protocol): Future[Unit] = {

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
                  }, protocol))
                }
              } else if (key.isReadable()) {
                clients get (key) map { _.readChunk }
              } else if (key.isWritable()) {
                unSelectForWrite(key)
                clients get (key) map { _.continueWriting }
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
    }
  }

  private def closeClient(key: SelectionKey) {
    key.channel().close()
    key.cancel()
    clients remove key
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

  def size = buffers map { _.limit } sum

  def buffersState: String = buffers map { b => s"${b.position} : ${b.limit}" } mkString "\n"
}

object ServerSpecs {
  def apply() = fromConfig(new Config())

  def fromConfig(conf: Config): ServerSpecs = {
    ServerSpecs(
      name = conf.string("server.name", "Shift-HTTPServer"),
      address = conf.string("server.address", "0.0.0.0"),
      port = conf.int("server.port", 8080),
      numThreads = conf.int("server.numThreads", Runtime.getRuntime.availableProcessors()))
  }
}

case class ServerSpecs(name: String,
                       address: String,
                       port: Int,
                       numThreads: Int)


