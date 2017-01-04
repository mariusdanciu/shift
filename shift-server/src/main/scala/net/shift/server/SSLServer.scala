package net.shift.server

import java.io.FileInputStream
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.security.KeyStore
import java.util.concurrent.Executors
import javax.net.ssl._

import net.shift.common.{Config, LogBuilder}
import net.shift.io.IO
import net.shift.server.Selections._
import net.shift.server.protocol.ProtocolBuilder

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object SSLServer {
  def apply() = new SSLServer(SSLServerSpecs())
}

case class SSLServer(specs: SSLServerSpecs) {

  private val log = LogBuilder.logger(classOf[SSLServer])

  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, SSLClientHandler]

  @volatile
  private var running = false

  def start(protocol: ProtocolBuilder): Future[Unit] = {

    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(specs.numThreads))

    @tailrec
    def loop(serverChannel: ServerSocketChannel) {
      if (running) {

        val r = selector.select()

        val keys = selector.selectedKeys().iterator()

        while (keys.hasNext) {
          val key = keys.next()
          keys.remove()

          if (!running) {
            closeClient(key)
          } else {
            if (key.isValid) {
              if (key.isAcceptable) {
                val client = serverChannel.accept()
                if (client != null) {
                  client.configureBlocking(false)

                  val sslEngine = makeSSLEngine()
                  sslEngine.beginHandshake()

                  if (handshake(client, sslEngine)) {
                    val clientKey = client.register(selector, SelectionKey.OP_READ)
                    val clientName = client.getRemoteAddress.toString + "-" + clientKey
                    log.info("Accepted connection " + clientName)
                    clients.put(clientKey, new SSLClientHandler(clientKey, sslEngine, clientName, k => {
                      closeClient(k)
                    }, protocol.createProtocol))
                  }
                }
              } else if (key.isReadable) {
                clients.get(key).foreach {
                  _.readChunk
                }
              } else if (key.isWritable) {
                unSelectForWrite(key)
                clients.get(key).foreach {
                  _.continueWriting()
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


  private def reallocate(buffer: ByteBuffer, capacity: Int):ByteBuffer = {
    if (capacity > buffer.capacity()) {
      ByteBuffer.allocate(capacity)
    } else {
      ByteBuffer.allocate(buffer.capacity() * 2)
    }
  }

  private def handshake(socket: SocketChannel, engine: SSLEngine)(implicit ctx: ExecutionContext): Boolean = {
    // https://github.com/alkarn/sslengine.example/tree/master/src/main/java/alkarn/github/io/sslengine/example
    val appBufferSize = engine.getSession.getApplicationBufferSize
    val packetSize = engine.getSession.getPacketBufferSize

    var clientDecryptedData = ByteBuffer.allocate(appBufferSize)
    var clientEncryptedData = ByteBuffer.allocate(packetSize)

    var serverDecryptedData = ByteBuffer.allocate(appBufferSize)
    var serverEncryptedData = ByteBuffer.allocate(packetSize)

    var handshakeStatus = engine.getHandshakeStatus
    while (handshakeStatus != SSLEngineResult.HandshakeStatus.FINISHED && handshakeStatus != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING) {
        case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
          if (socket.read(clientEncryptedData) < 0) {
            if (engine.isInboundDone && engine.isOutboundDone) {
              handshakeStatus = SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING
            } else {
              engine.closeInbound()
              engine.closeOutbound()

              // After closeOutbound the engine will be set to WRAP state, in order to try to send a close message to the client.
              handshakeStatus = engine.getHandshakeStatus
            }
          } else {
            clientEncryptedData.flip()
            val result = engine.unwrap(clientEncryptedData, clientDecryptedData)
            clientEncryptedData.compact()
            handshakeStatus = result.getHandshakeStatus
            result.getStatus match {
              case SSLEngineResult.Status.OK =>
              case SSLEngineResult.Status.CLOSED =>
                engine.closeOutbound()
                handshakeStatus = engine.getHandshakeStatus
              case SSLEngineResult.Status.BUFFER_OVERFLOW =>
                clientDecryptedData = reallocate(clientDecryptedData, appBufferSize)
              case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
                clientEncryptedData = reallocate(clientEncryptedData, packetSize)

            }
          }
        case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
          serverEncryptedData.clear()
          val result = engine.wrap(serverDecryptedData, serverEncryptedData)
          handshakeStatus = result.getHandshakeStatus
          result.getStatus match {
            case SSLEngineResult.Status.OK =>
            case SSLEngineResult.Status.CLOSED =>
              serverEncryptedData.flip()
              while (serverEncryptedData.hasRemaining) {
                socket.write(serverEncryptedData)
              }
              clientDecryptedData.clear()
            case SSLEngineResult.Status.BUFFER_OVERFLOW =>
              serverEncryptedData = reallocate(serverEncryptedData, packetSize)
            case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
              clientEncryptedData = reallocate(clientEncryptedData, packetSize)

          }

        case SSLEngineResult.HandshakeStatus.NEED_TASK =>
          var task = engine.getDelegatedTask
          while (task != null) {
            ctx.execute(task)
            task = engine.getDelegatedTask
          }
          handshakeStatus = engine.getHandshakeStatus
    }
    true
  }

  private def makeSSLEngine(): SSLEngine = {
    val ks = KeyStore.getInstance("JKS")
    val ts = KeyStore.getInstance("JKS")

    val passPhrase = specs.pass.toCharArray
    ks.load(new FileInputStream(specs.keyStoreFile), passPhrase)
    ts.load(new FileInputStream(specs.trustStoreFile), passPhrase)

    val kmf = KeyManagerFactory.getInstance("SunX509")
    kmf.init(ks, passPhrase)

    val tmf = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ts)

    val sslCtx = SSLContext.getInstance("TLS")

    sslCtx.init(kmf.getKeyManagers, tmf.getTrustManagers, null)

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

  def stop(): Selector = {
    running = false
    selector.wakeup()
  }

}

object SSLServerSpecs {
  def apply(): SSLServerSpecs = fromConfig(Config())

  def fromConfig(conf: Config): SSLServerSpecs = {
    SSLServerSpecs(
      name = conf.string("server.name", "Shift-HTTPServer"),
      address = conf.string("server.address", "0.0.0.0"),
      port = conf.int("server.port", 8080),
      numThreads = conf.int("server.numThreads", Runtime.getRuntime.availableProcessors()),
      keyStoreFile = conf.string("server.keystore", "keystore.jks"),
      trustStoreFile = conf.string("server.truststore", "truststore.ts"),
      pass = conf.string("server.pass"))
  }
}

case class SSLServerSpecs(name: String,
                          address: String,
                          port: Int,
                          numThreads: Int,
                          keyStoreFile: String,
                          trustStoreFile: String,
                          pass: String)


