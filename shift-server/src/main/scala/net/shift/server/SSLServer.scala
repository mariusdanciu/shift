package net.shift.server

import java.io.FileInputStream
import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.security.KeyStore
import java.util.concurrent.Executors
import javax.net.ssl._

import net.shift.common.{Config, Log}
import net.shift.io.IO
import net.shift.server.Selections._
import net.shift.server.protocol.ProtocolBuilder

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object SSLServer {
  def apply() = new SSLServer(SSLServerSpecs())
}

case class SSLServer(specs: SSLServerSpecs) extends Log {

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, SSLClientHandler]

  def loggerName = classOf[Server].getName

  @volatile
  private var running = false;

  def start(protocol: ProtocolBuilder): Future[Unit] = {

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

                  val sslEngine = makeSSLEngine()
                  sslEngine.beginHandshake()

                  if (handshake(sslEngine)) {
                    val clientKey = client.register(selector, SelectionKey.OP_READ)
                    val clientName = client.getRemoteAddress.toString + "-" + clientKey
                    log.info("Accepted connection " + clientName)
                    clients.put(clientKey, new SSLClientHandler(clientKey, sslEngine, clientName, k => {
                      closeClient(k)
                    }, protocol.createProtocol))
                  }
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

  private def handshake(engine: SSLEngine): Boolean = {
    // https://github.com/alkarn/sslengine.example/tree/master/src/main/java/alkarn/github/io/sslengine/example
    val handshakeStatus = engine.getHandshakeStatus();
    while (handshakeStatus != SSLEngineResult.HandshakeStatus.FINISHED && handshakeStatus != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING) {
      handshakeStatus match {
        case SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING =>
          false
        case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
          false
        case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
          false
        case SSLEngineResult.HandshakeStatus.NEED_TASK =>
          false
        case SSLEngineResult.HandshakeStatus.FINISHED =>
          true
      }
    }
    true
  }

  private def makeSSLEngine(): SSLEngine = {
    val ks = KeyStore.getInstance("JKS")
    val ts = KeyStore.getInstance("JKS")

    val passPhrase = specs.pass.toCharArray()
    ks.load(new FileInputStream(specs.keyStoreFile), passPhrase)
    ts.load(new FileInputStream(specs.trustStoreFile), passPhrase)

    val kmf = KeyManagerFactory.getInstance("SunX509")
    kmf.init(ks, passPhrase)

    val tmf = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ts)

    val sslCtx = SSLContext.getInstance("TLS")

    sslCtx.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null)

    val engine = sslCtx.createSSLEngine()
    engine.setUseClientMode(false)

    engine
  }

  private def closeClient(key: SelectionKey) {
    IO.close(key.channel())
    key.cancel()
    val state = clients remove key
    log.info(s"Client $key removed: $state")
  }

  def stop() = {
    running = false;
    selector.wakeup()
  }

}

object SSLServerSpecs {
  def apply() = fromConfig(Config())

  def fromConfig(conf: Config): SSLServerSpecs = {
    SSLServerSpecs(
      name = conf.string("server.name", "Shift-HTTPServer"),
      address = conf.string("server.address", "0.0.0.0"),
      port = conf.int("server.port", 8080),
      numThreads = conf.int("server.numThreads", Runtime.getRuntime.availableProcessors()),
      keyStoreFile = conf.string("server.keystore", "keystore.jks"),
      trustStoreFile = conf.string("server.truststore", "truststore.ts"),
      pass = conf.string("server.pass", ""))
  }
}

case class SSLServerSpecs(name: String,
                          address: String,
                          port: Int,
                          numThreads: Int,
                          keyStoreFile: String,
                          trustStoreFile: String,
                          pass: String)


