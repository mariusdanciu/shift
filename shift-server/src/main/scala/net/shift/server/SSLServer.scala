package net.shift.server

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.util.concurrent.Executors

import net.shift.common.{Config, LogBuilder}
import net.shift.io.IO
import net.shift.server.Selections._
import net.shift.server.protocol.ProtocolBuilder

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object SSLServer {
  def apply() = new SSLServer(ServerSpecs())
}

case class SSLServer(specs: ServerSpecs) extends SSLOps with KeyLogger {
  protected val log = LogBuilder.logger(classOf[SSLServer])

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, SSLClientHandler]

  @volatile
  private var running = false

  def start(protocol: ProtocolBuilder): Future[Unit] = {

    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(specs.numThreads))

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
                  val ch = new SSLClientHandler(clientKey, specs, clientName, k => {
                    closeClient(k)
                  }, protocol.createProtocol)

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
    val address = new InetSocketAddress(specs.address, specs.port)
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

object ServerSpecs {
  def apply(): ServerSpecs = fromConfig(Config())

  def apply(name: String,
            address: String,
            port: Int,
            numThreads: Int): ServerSpecs = ServerSpecs(
    name,
    address,
    port,
    numThreads,
    SSLSpecs("", "", "")
  )

  def fromConfig(conf: Config): ServerSpecs = {

    ServerSpecs(
      name = conf.string("server.name", "Shift-HTTPServer"),
      address = conf.string("server.address", "0.0.0.0"),
      port = conf.int("server.port", 8080),
      numThreads = conf.int("server.numThreads", Runtime.getRuntime.availableProcessors()),
      ssl = SSLSpecs(conf.string("server.keystore", "keystore.jks"),
        conf.string("server.keystore", "keystore.jks"),
        conf.string("server.pass"))
    )
  }
}

case class ServerSpecs(name: String,
                       address: String,
                       port: Int,
                       numThreads: Int,
                       ssl: SSLSpecs)

case class SSLSpecs(keyStoreFile: String,
                    trustStoreFile: String,
                    pass: String)
