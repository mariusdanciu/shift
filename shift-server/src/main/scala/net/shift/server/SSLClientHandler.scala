package net.shift.server

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}
import java.security.KeyStore
import java.util.concurrent.Executors
import javax.net.ssl._

import net.shift.common.{Log, LogBuilder}
import net.shift.io._
import net.shift.server.Selections._
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext
import scala.util.Try

private[server] case class SSLClientHandler(key: SelectionKey,
                                            sslConfig: SSLConfig,
                                            onClose: SelectionKey => Unit,
                                            protocol: Protocol) extends SSLOps with ConnectionHandler with KeyLogger {
  protected val log: Log = LogBuilder.logger(classOf[SSLClientHandler])


  private val engine = makeSSLEngine()
  private val appBufferSize = engine.getSession.getApplicationBufferSize
  private val packetSize = engine.getSession.getPacketBufferSize


  var clientDecryptedData: ByteBuffer = ByteBuffer.allocate(appBufferSize)
  var clientEncryptedData: ByteBuffer = ByteBuffer.allocate(packetSize)

  var serverDecryptedData: ByteBuffer = ByteBuffer.allocate(appBufferSize)
  var serverEncryptedData: ByteBuffer = ByteBuffer.allocate(packetSize)

  var handshaking = true

  override def start():Unit = {
    engine.beginHandshake()
  }

  def handleRead()(implicit ec: ExecutionContext): Unit = {
    if (handshaking) {
      handshakeRead().recover {
        case t =>
          keyLog(key, t.getMessage)
          terminate()
      }
    } else {
      readChunk()
    }
  }

  def handleWrite()(implicit ec: ExecutionContext): Unit = {
    if (handshaking) {
      handshakeWrite().recover {
        case t =>
          keyLog(key, t.getMessage)
          terminate()
      }
    } else {
      continueSending(drain)
    }
  }

  private def makeSSLEngine(): SSLEngine = {
    val ks = KeyStore.getInstance("JKS")
    val ts = KeyStore.getInstance("JKS")

    val passPhrase = sslConfig.pass.toCharArray
    ks.load(new FileInputStream(sslConfig.keyStoreFile), passPhrase)
    ts.load(new FileInputStream(sslConfig.trustStoreFile), passPhrase)

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

  private def handshakeRead(): Try[SelectionKey] = Try {
    val client = key.channel().asInstanceOf[SocketChannel]
    var handshakeStatus = engine.getHandshakeStatus
    keyLog(key, "handshakeRead Handshake status " + handshakeStatus)

    handshakeStatus match {
      case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
        unSelectForRead(key)
        selectForWrite(key)

      case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
        selectForRead(key)
        val read = client.read(clientEncryptedData)
        keyLog(key, "Unwrap read from socket clientEncryptedData " + clientEncryptedData)
        if (read < 0) {
          if (engine.isInboundDone && engine.isOutboundDone) {
            // STOP
          } else {
            try {
              engine.closeInbound()
            } catch {
              case e: Throwable =>
                keyLog(key, "This engine was forced to close inbound, without having received the proper SSL/TLS close notification message from the peer, due to end of stream.");
            }
            engine.closeOutbound()
          }
        }

        clientEncryptedData.flip()

        while (clientEncryptedData.hasRemaining && handshakeStatus == SSLEngineResult.HandshakeStatus.NEED_UNWRAP) {

          val unwrapStatus = unwrap(key, engine, clientEncryptedData, clientDecryptedData)

          keyLog(key, "Unwrap status: " + unwrapStatus)


          unwrapStatus match {
            case OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, src, _) =>
              clientEncryptedData = src

            case OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, _, dest) =>
              clientDecryptedData = dest

            case OPResult(SSLEngineResult.Status.CLOSED, out, dest) =>
              out.flip()
              keyLog(key, "CLOSE Sending " + out)
              while (out.hasRemaining) {
                client.write(out)
              }
              terminate()
              keyLog(key, "CLOSE Sent " + out)
            case _ =>
          }

          var done = false
          while (!done) {

            val hsStatus = engine.getHandshakeStatus
            keyLog(key, "handshakeRead loop Handshake status " + hsStatus)
            hsStatus match {
              case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
                selectForWrite(key)
                done = true
              case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
                selectForRead(key)
                done = true
              case SSLEngineResult.HandshakeStatus.NEED_TASK =>
                handleNeedTask(key)
              case _ =>
                done = true

            }
          }

          handshakeStatus = engine.getHandshakeStatus
        }

        clientEncryptedData.compact()

      case SSLEngineResult.HandshakeStatus.FINISHED | SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING =>
        keyLog(key, "Handshake successful")
        handshaking = false
      case s =>
        keyLog(key, s"Unexpected state $s")
    }
    key
  }

  private def handleNeedTask(key: SelectionKey) = {
    val exec = Executors.newSingleThreadExecutor()
    var task = engine.getDelegatedTask
    while (task != null) {
      keyLog(key, "Schedule SSL Task - " + task)
      exec.execute(task)
      task = engine.getDelegatedTask
    }
  }

  private def handshakeWrite()(implicit ec: ExecutionContext): Try[SelectionKey] = Try {
    val client = key.channel().asInstanceOf[SocketChannel]
    val handshakeStatus = engine.getHandshakeStatus
    keyLog(key, "handshakeWrite loop Handshake status " + handshakeStatus)

    handshakeStatus match {
      case SSLEngineResult.HandshakeStatus.NEED_WRAP =>

        serverEncryptedData.clear()
        val writeStatus = wrap(key, engine, serverDecryptedData, serverEncryptedData)
        keyLog(key, "Write status: " + writeStatus)

        writeStatus match {
          case OPResult(SSLEngineResult.Status.OK, out, _) =>
            out.flip()
            keyLog(key, "OK Sending " + out)
            while (out.hasRemaining) {
              client.write(out)
            }
            keyLog(key, "OK Sent " + out)
          case OPResult(SSLEngineResult.Status.CLOSED, out, _) =>
            out.flip()
            keyLog(key, "CLOSE Sending " + out)
            while (out.hasRemaining) {
              client.write(out)
            }
            terminate()
            keyLog(key, "CLOSE Sent " + out)
        }

        var done = false
        while (!done) {
          val hsStatus = engine.getHandshakeStatus
          keyLog(key, "handshakeWrite loop Handshake status " + hsStatus)
          hsStatus match {
            case SSLEngineResult.HandshakeStatus.NEED_WRAP =>
              selectForWrite(key)
              done = true
            case SSLEngineResult.HandshakeStatus.NEED_UNWRAP =>
              selectForRead(key)
              done = true
            case SSLEngineResult.HandshakeStatus.NEED_TASK =>
              handleNeedTask(key)
            case SSLEngineResult.HandshakeStatus.FINISHED | SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING =>
              done = true
              selectForRead(key)
              handshaking = false
              keyLog(key, "Handshake successful")
              handleExistentApplicationData(key)
          }
        }

      case status =>
        keyLog(key, "Got handshake status: " + status)
    }
    key
  }

  private def handleExistentApplicationData(key: SelectionKey)(implicit ec: ExecutionContext) = {
    keyLog(key, "Try handle app data")
    val clientBuf = clientEncryptedData
    keyLog(key, "client buf " + clientBuf)
    if (clientBuf.position() > 0) {
      readEncryptedBuffer(clientBuf, buf => {
        keyLog(key, "Decrypted buf " + buf)
        protocol(buf) {
          resp =>
            keyLog(key, "Response " + resp)
            sendResponse(resp)
        }
      })
    }
  }

  def terminate(): Unit = {
    if (!engine.isInboundDone)
      Try(engine.closeInbound())

    if (!engine.isOutboundDone)
      Try(engine.closeOutbound())

    onClose(key)
  }


  def readEncryptedBuffer(enc: ByteBuffer, f: ByteBuffer => Unit): Unit = {
    var clientEncryptedData = enc
    keyLog(key, "clientEncryptedData " + clientEncryptedData)
    clientEncryptedData.flip()
    keyLog(key, "clientEncryptedData " + clientEncryptedData)
    var goOn = clientEncryptedData.hasRemaining
    while (goOn) {
      clientDecryptedData.clear()

      unwrap(key, engine, clientEncryptedData, clientDecryptedData) match {
        case OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, src, _) =>
          clientEncryptedData = src
          goOn = clientEncryptedData.hasRemaining
        case OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, _, dest) =>
          clientDecryptedData = dest
          goOn = clientEncryptedData.hasRemaining
        case OPResult(SSLEngineResult.Status.OK, _, dec) =>
          dec.flip()
          f(dec)
          goOn = clientEncryptedData.hasRemaining
        case OPResult(SSLEngineResult.Status.CLOSED, _, _) =>
          val status = engine.getHandshakeStatus
          if (status == SSLEngineResult.HandshakeStatus.NEED_WRAP) {
            serverEncryptedData.clear()
            val wrapResult = wrap(key, engine, serverDecryptedData, serverEncryptedData)
            keyLog(key, "CLOSED WRAP Result" + wrapResult)
          }
          terminate()
          goOn = false
      }

    }
  }

  private def readBuf(client: SocketChannel, f: ByteBuffer => Unit) {
    clientEncryptedData.clear()
    val size = client.read(clientEncryptedData)
    keyLog(key, s"Read $size bytes")

    if (size < 0) {
      keyLog(key, "End of client stream")
      terminate()
    } else if (size == 0) {
      keyLog(key, "No data to read")
    } else {
      readEncryptedBuffer(clientEncryptedData, f)
    }
  }


  def readChunk()(implicit ctx: ExecutionContext) {
    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      readBuf(client, buf => {
        protocol(buf) {
          resp => sendResponse(resp)
        }
      })

    }.recover {
      case e =>
        log.error("Cannot read data from client: ", e)
        terminate()
    }
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    def write(b: ByteBuffer) = {
      b.flip()
      keyLog(key, "Sending encrypted buf " + b)
      var written = client.write(b)
      keyLog(key, " - response: wrote " + written)
      while (written > 0 && b.hasRemaining) {
        written = client.write(b)
        keyLog(key, " - response: wrote " + written)
      }
      (written, b)
    }

    serverEncryptedData.clear()
    wrap(key, engine, buffer, serverEncryptedData) match {
      case OPResult(SSLEngineResult.Status.OK, b, _) =>
        write(b)
      case OPResult(SSLEngineResult.Status.CLOSED, b, _) =>
        val res = write(b)
        terminate()
        res
      case r => throw new Exception(s"Cannot send data $r")
    }
  }

  def sendResponse(resp: BinProducer):Unit = {
    writeState = Some(ResponseContinuationState(resp))
    continueSending(drain)
  }

}

