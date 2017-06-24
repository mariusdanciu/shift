package net.shift.server

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.util.concurrent.Executors

import net.shift.common.{Config, Log, LogBuilder}
import net.shift.io.IO
import net.shift.server.Selections._
import net.shift.server.protocol.ProtocolBuilder

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object Server {
  def apply() = new Server(ServerConfig(), None)

  def apply(cfg: ServerConfig) = new Server(cfg, None)

  def apply(cfg: ServerConfig, sslConfig: SSLConfig) = new Server(cfg, Some(sslConfig))
}

case class Server(config: ServerConfig, sslConfig: Option[SSLConfig]) extends KeyLogger {
  protected val log: Log = LogBuilder.logger(classOf[Server])

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, ConnectionHandler]

  @volatile
  private var running = false

  def start(protocol: ProtocolBuilder): Future[Unit] = {

    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(config.numThreads))

    def makeConnectionHandler(clientKey: SelectionKey): ConnectionHandler = {
      sslConfig match {
        case Some(ssl) =>
          SSLClientHandler(clientKey, ssl, k => {
            closeClient(k)
          }, protocol.createProtocol)
        case _ =>
          ClientHandler(clientKey, k => {
            closeClient(k)
          }, protocol.createProtocol)
      }
    }

    @tailrec
    def loop(serverChannel: ServerSocketChannel) {
      if (running) {
        log.debug("Waiting for key")
        val k = selector.select()
        log.debug(s"Got selector key $k")

        val keys = selector.selectedKeys().iterator()

        while (keys.hasNext) {
          val key = keys.next()
          keys.remove()

          keyLog(key, "SELECTED")


          if (!running) {
            closeClient(key)
          } else {
            if (key.isValid) {
              if (key.isAcceptable) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)
                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  val clientName = client.getRemoteAddress.toString + "-" + clientKey

                  keyLog(key, "Accepted connection " + clientName)
                  val ch = makeConnectionHandler(clientKey)

                  ch.start()

                  clients.put(clientKey, ch)

                }
              } else if (key.isReadable) {
                clients.get(key).foreach { state =>
                  state.handleRead()
                }
              } else if (key.isWritable) {
                unSelectForWrite(key)
                clients.get(key).foreach { state =>
                  state.handleWrite()
                }
              }
            }
          }
        }
        loop(serverChannel)
      }
    }

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    val address = new InetSocketAddress(config.address, config.port)
    serverChannel.bind(address)
    log.info("Server bound to " + address)

    serverChannel.register(selector, SelectionKey.OP_ACCEPT, null)

    running = true

    val listen = Future {
      loop(serverChannel)
    }

    listen.map { _ =>
      log.info("Shutting down server")
      serverChannel.close()
    }
  }

  private def closeClient(key: SelectionKey) {
    IO.close(key.channel())
    key.cancel()
    val state = clients remove key
    log.info(s"Client $key removed: $state")
  }

  def stop(): Selector = {
    running = false
    selector.wakeup()
  }

}

object ServerConfig {
  def apply(): ServerConfig = fromConfig(Config())


  def fromConfig(conf: Config): ServerConfig = {

    ServerConfig(
      name = conf.string("server.name", "Shift-HTTPServer"),
      address = conf.string("server.address", "0.0.0.0"),
      port = conf.int("server.port", 8080),
      numThreads = conf.int("server.numThreads", Runtime.getRuntime.availableProcessors())
    )
  }
}

case class ServerConfig(name: String,
                        address: String,
                        port: Int,
                        numThreads: Int)


object SSLConfig {
  def apply(): SSLConfig = fromConfig(Config())


  def fromConfig(conf: Config): SSLConfig = {

    SSLConfig(conf.string("server.keystore", "keystore.jks"),
      conf.string("server.keystore", "keystore.jks"),
      conf.string("server.pass"))
  }
}

case class SSLConfig(keyStoreFile: String,
                     trustStoreFile: String,
                     pass: String)
