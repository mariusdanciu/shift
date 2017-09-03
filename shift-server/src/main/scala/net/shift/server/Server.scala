package net.shift.server

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.util.concurrent.Executors

import net.shift.common.{Config, Log, LogBuilder}
import net.shift.io.IO
import net.shift.server.Selections._
import net.shift.server.ServerConfigNames._
import net.shift.server.http.{HttpProtocolBuilder, HttpService}
import net.shift.server.protocol.ProtocolBuilder

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object HttpServer {

  def apply(config: Config, service: HttpService): Server = {
    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(config.int(`server.port`, 80)))
    Server(config, HttpProtocolBuilder(service), ssl = false)
  }

  def apply(port: Int, threads: Int, service: HttpService): Server = {
    val c = Config(
      `server.address` -> "0.0.0.0",
      `server.port` -> s"$port",
      `server.numThreads` -> s"$threads"
    )
    HttpServer(c, service)
  }

  def apply(host: String, port: Int, threads: Int, service: HttpService): Server = {
    val c = Config(
      `server.address` -> host,
      `server.port` -> s"$port",
      `server.numThreads` -> s"$threads"
    )
    HttpServer(c, service)
  }

}

object HttpsServer {
  def apply(config: Config, service: HttpService): Server = {
    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(config.int(`server.port`, 443)))
    Server(config, HttpProtocolBuilder(service), ssl = true)
  }

  def apply(port: Int, threads: Int, keystorePass: String, service: HttpService): Server = {
    val c = Config(
      `server.address` -> "0.0.0.0",
      `server.ssl.port` -> s"$port",
      `server.ssl.numThreads` -> s"$threads",
      `server.ssl.keystore` -> ".keystore",
      `server.ssl.truststore` -> ".truststore",
      `server.ssl.pass` -> s"$keystorePass"
    )
    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threads))
    HttpsServer(c, service)
  }

  def apply(host: String, port: Int, threads: Int, keystorePass: String, service: HttpService): Server = {
    val c = Config(
      `server.address` -> host,
      `server.ssl.port` -> s"$port",
      `server.ssl.numThreads` -> s"$threads",
      `server.ssl.keystore` -> ".keystore",
      `server.ssl.truststore` -> ".truststore",
      `server.ssl.pass` -> s"$keystorePass"
    )
    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threads))
    HttpsServer(c, service)
  }

}

case class Server(config: Config, protocol: ProtocolBuilder, ssl: Boolean, listener: Option[ServerListener] = None)(implicit ctx: ExecutionContext) extends KeyLogger {
  protected val log: Log = LogBuilder.logger(classOf[Server])

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, ConnectionHandler]

  @volatile
  private var running = false

  def withListener(l: ServerListener) = copy(listener = Some(l))

  def start(): Future[Unit] = {

    def makeConnectionHandler(clientKey: SelectionKey): ConnectionHandler = {
      if (ssl)
        SSLClientHandler(clientKey, config, k => {
          closeClient(k)
        }, protocol.createProtocol,
          listener)
      else
        ClientHandler(clientKey, k => {
          closeClient(k)
        }, protocol.createProtocol,
          1024,
          listener)
    }

    @tailrec
    def loop(serverChannel: ServerSocketChannel): Unit = {
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

    val address = if (ssl) {
      new InetSocketAddress(config.string(`server.address`, "localhost"), config.int(`server.ssl.port`, 8443))
    } else {
      new InetSocketAddress(config.string(`server.address`, "localhost"), config.int(`server.port`, 8080))
    }

    serverChannel.bind(address)
    log.info("Server bound to " + address)

    serverChannel.register(selector, SelectionKey.OP_ACCEPT, null)

    running = true
    listener.map {
      _.onStart()
    }
    val listen = Future {
      loop(serverChannel)
    }

    listen.recover {
      case t => listener.map(_.onError(t))
    }.map { _ =>
      log.info("Shutting down server")
      serverChannel.close()
      listener.map(_.onShutDown())
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

object ServerConfigNames {
  val `server.address` = "server.address"
  val `server.port` = "server.port"
  val `server.numThreads` = "server.numThreads"

  val `server.ssl.port` = "server.ssl.port"
  val `server.ssl.numThreads` = "server.ssl.numThreads"
  val `server.ssl.keystore` = "server.ssl.keystore"
  val `server.ssl.truststore` = "server.ssl.truststore"
  val `server.ssl.pass` = "server.ssl.pass"
}


trait ServerListener {
  def onStart(): Unit

  def onShutDown(): Unit

  def onError(t: Throwable)
}