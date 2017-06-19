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
import scala.util.{Failure, Success}

object SSLServer {
  def apply() = new SSLServer(SSLServerSpecs())
}

case class SSLServer(specs: SSLServerSpecs) extends SSLOps {


  private val selector = Selector.open

  private val clients = new TrieMap[SelectionKey, SSLState]

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

                  val sslEngine = makeSSLEngine()
                  sslEngine.beginHandshake()

                  val clientKey = client.register(selector, SelectionKey.OP_READ)
                  val clientName = client.getRemoteAddress.toString + "-" + clientKey
                  keyLog(key, "Accepted connection " + clientName)
                  val ch = new SSLClientHandler(clientKey, sslEngine, clientName, k => {
                    closeClient(k)
                  }, protocol.createProtocol)

                  clients.put(clientKey, SSLState(true, ch))

                }
              } else if (key.isReadable) {
                clients.get(key).foreach { state =>
                  if (state.handshaking) {
                    handshakeRead(key, state)
                  } else {
                    state.handler.readChunk
                  }
                }
              } else if (key.isWritable) {
                unSelectForWrite(key)
                clients.get(key).foreach { state =>
                  if (state.handshaking) {
                    handshakeWrite(key, state)
                  } else {
                    state.handler.continueWriting
                  }
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
      try {
        loop(serverChannel)
      } catch {
        case t: Throwable => t.printStackTrace()
      }
    }

    listen.map { _ =>
      log.info("Shutting down server")
      serverChannel.close()
    }
  }

  private def handleExistentApplicationData(key: SelectionKey, state: SSLState)(implicit ec: ExecutionContext) = {
    keyLog(key, "Try handle app data")
    val clientBuf = state.handler.clientEncryptedData
    val ch = state.handler
    keyLog(key, "client buf " + clientBuf)
    if (clientBuf.position() > 0) {
      ch.readEncryptedBuffer(clientBuf, buf => {
        keyLog(key, "Decrypted buf " + buf)
        ch.protocol(buf) {
          (resp, rid) =>
            keyLog(key, "Response " + resp)
            ch.send(Some(ResponseContinuationState(resp, rid)))
        }
      })
    }
  }

  private def handshakeWrite(key: SelectionKey, state: SSLState)(implicit ec: ExecutionContext) = {
    val client = key.channel().asInstanceOf[SocketChannel]
    val handshakeStatus = state.handler.engine.getHandshakeStatus
    keyLog(key, "Handshake status " + handshakeStatus)

    handshakeStatus match {
      case SSLEngineResult.HandshakeStatus.NEED_WRAP =>

        state.handler.serverEncryptedData.clear()
        val writeStatus = wrap(key, state.handler.engine, state.handler.serverDecryptedData, state.handler.serverEncryptedData)
        keyLog(key, "Write status: " + writeStatus)

        writeStatus match {
          case OPResult(SSLEngineResult.Status.OK, out, _) =>
            out.flip()
            keyLog(key, "OK Sending " + out)
            while (out.hasRemaining()) {
              client.write(out)
            }
            keyLog(key, "OK Sent " + out)
          case OPResult(SSLEngineResult.Status.CLOSED, out, _) =>
            out.flip()
            keyLog(key, "CLOSE Sending " + out)
            while (out.hasRemaining()) {
              client.write(out)
            }
            state.handler.engine.closeOutbound()
            keyLog(key, "CLOSE Sent " + out)
          // STOP

        }

        var done = false
        while (!done) {
          val hsStatus = state.handler.engine.getHandshakeStatus
          keyLog(key, "Handshake status " + hsStatus)
          hsStatus match {
            case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
              selectForWrite(key)
              done = true
            case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
              selectForRead(key)
              done = true
            case SSLEngineResult.HandshakeStatus.NEED_TASK =>
              handleNeedTask(key, state)
            case SSLEngineResult.HandshakeStatus.FINISHED | SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING =>
              done = true
              selectForRead(key)
              keyLog(key, "Handshake successful")
              clients.put(key, state.copy(handshaking = false))
              handleExistentApplicationData(key, state)
          }
        }
    }


  }


  private def handshakeRead(key: SelectionKey, state: SSLState) = {
    val client = key.channel().asInstanceOf[SocketChannel]
    var handshakeStatus = state.handler.engine.getHandshakeStatus
    keyLog(key, "Handshake status " + handshakeStatus)

    handshakeStatus match {
      case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
        val read = client.read(state.handler.clientEncryptedData)
        keyLog(key, "Unwrap read from socket clientEncryptedData " + state.handler.clientEncryptedData)
        if (read < 0) {
          if (state.handler.engine.isInboundDone && state.handler.engine.isOutboundDone) {
            // STOP
          } else {
            try {
              state.handler.engine.closeInbound()
            } catch {
              case e: Throwable =>
                keyLog(key, "This engine was forced to close inbound, without having received the proper SSL/TLS close notification message from the peer, due to end of stream.");
            }
            state.handler.engine.closeOutbound()
          }
        }

        state.handler.clientEncryptedData.flip()

        while (state.handler.clientEncryptedData.hasRemaining && handshakeStatus == SSLEngineResult.HandshakeStatus.NEED_UNWRAP) {

          val unwrapStatus = unwrap(key, state.handler.engine, state.handler.clientEncryptedData, state.handler.clientDecryptedData)

          keyLog(key, "Unwrap status: " + unwrapStatus)


          unwrapStatus match {
            case OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, src, _) =>
              state.handler.clientEncryptedData = src

            case OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, _, dest) =>
              state.handler.clientDecryptedData = dest

            case OPResult(SSLEngineResult.Status.CLOSED, _, dest) =>
              if (state.handler.engine.isOutboundDone()) {
                // STOP
              } else {
                state.handler.engine.closeOutbound()
              }
            case _ =>
          }

          var done = false
          while (!done) {

            val hsStatus = state.handler.engine.getHandshakeStatus
            keyLog(key, "Handshake status " + hsStatus)
            hsStatus match {
              case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
                selectForWrite(key)
                done = true
              case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
                selectForRead(key)
                done = true
              case SSLEngineResult.HandshakeStatus.NEED_TASK =>
                handleNeedTask(key, state)
              case _ =>
                done = true

            }
          }

          handshakeStatus = state.handler.engine.getHandshakeStatus
        }

        state.handler.clientEncryptedData.compact()

      case SSLEngineResult.HandshakeStatus.FINISHED | SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING =>
        keyLog(key, "Handshake successful")
        clients.put(key, state.copy(handshaking = false))
    }
  }

  private def handleNeedTask(key: SelectionKey, state: SSLState) = {
    val engine = state.handler.engine
    val exec = Executors.newSingleThreadExecutor()
    var task = engine.getDelegatedTask
    while (task != null) {
      keyLog(key, "Schedule SSL Task - " + task)
      exec.execute(task)
      task = engine.getDelegatedTask
    }
  }

  private def handshake(socket: SocketChannel, engine: SSLEngine): (Boolean, ByteBuffer) = {
    // https://github.com/alkarn/sslengine.example/tree/master/src/main/java/alkarn/github/io/sslengine/example
    val appBufferSize = engine.getSession.getApplicationBufferSize
    val packetSize = engine.getSession.getPacketBufferSize

    var clientDecryptedData = ByteBuffer.allocate(appBufferSize)
    var clientEncryptedData = ByteBuffer.allocate(packetSize)

    val serverDecryptedData = ByteBuffer.allocate(appBufferSize)
    val serverEncryptedData = ByteBuffer.allocate(packetSize)

    var handshakeStatus = engine.getHandshakeStatus

    var forceDone = false
    while (handshakeStatus != SSLEngineResult.HandshakeStatus.FINISHED &&
      handshakeStatus != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING && !forceDone) {

      log.debug("HS Status " + handshakeStatus)
      handshakeStatus match {
        case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
          val read = socket.read(clientEncryptedData)
          log.debug("HS Read " + read + " bytes")
          if (read < 0) {
            if (engine.isInboundDone && engine.isOutboundDone) {
              forceDone = true
            } else {
              try {
                engine.closeInbound()
              } catch {
                case e: Throwable =>
                  log.error("This engine was forced to close inbound, without having received the proper SSL/TLS close notification message from the peer, due to end of stream.");
              }
              engine.closeOutbound()
            }
          }

          clientEncryptedData.flip()

          unwrap(null, engine, clientEncryptedData, clientDecryptedData) match {
            case OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, src, _) =>
              clientEncryptedData = src
            case OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, _, dest) =>
              clientDecryptedData = dest
            case OPResult(SSLEngineResult.Status.CLOSED, _, dest) =>
              log.info("UNWRAP CLOSE")

              if (engine.isOutboundDone()) {
                forceDone = true
              } else {
                engine.closeOutbound()
              }
            case _ =>
          }

          clientEncryptedData.compact()

        case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
          serverEncryptedData.clear()
          val writeStatus = wrap(null, engine, serverDecryptedData, serverEncryptedData)
          log.info("Write status: " + writeStatus)

          writeStatus match {
            case OPResult(SSLEngineResult.Status.OK, out, _) =>
              out.flip()
              println("OK Sending " + out)
              while (out.hasRemaining()) {
                socket.write(out)
              }
              println("OK Sent " + out)
            case OPResult(SSLEngineResult.Status.CLOSED, out, _) =>
              out.flip()
              println("CLOSE Sending " + out)
              while (out.hasRemaining()) {
                socket.write(out)
              }
              engine.closeOutbound()
              forceDone = true
              println("CLOSE Sent " + out)

          }

        case SSLEngineResult.HandshakeStatus.NEED_TASK =>
          val exec = Executors.newSingleThreadExecutor()
          var task = engine.getDelegatedTask
          while (task != null) {
            exec.execute(task)
            task = engine.getDelegatedTask
          }

        case _ =>
      }
      handshakeStatus = engine.getHandshakeStatus
    }

    log.info("serverDecryptedData " + serverDecryptedData)
    log.info("serverEncryptedData " + serverEncryptedData)

    log.info("clientEncryptedData " + clientEncryptedData)
    log.info("clientDecryptedData " + clientDecryptedData)

    log.info("handshakeStatus: " + handshakeStatus)

    val state = (handshakeStatus == SSLEngineResult.HandshakeStatus.FINISHED || handshakeStatus == SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING) && !forceDone
    (state, clientEncryptedData)
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
    engine.setNeedClientAuth(false)
    engine.setWantClientAuth(false)

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


case class SSLState(handshaking: Boolean, handler: SSLClientHandler)
