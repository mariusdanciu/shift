package net.shift.server

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}
import javax.net.ssl.{SSLEngine, SSLEngineResult}

import net.shift.common.LogBuilder
import net.shift.io.{Iteratee, _}
import net.shift.server.Selections._
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

private[server] case class SSLClientHandler(key: SelectionKey,
                                            engine: SSLEngine,
                                            name: String,
                                            onClose: SelectionKey => Unit,
                                            protocol: Protocol,
                                            readBufSize: Int = 1024) extends SSLOps with KeyLogger {
  protected val log = LogBuilder.logger(classOf[SSLClientHandler])

  private var writeState: Option[ResponseContinuationState] = None

  private val appBufferSize = engine.getSession.getApplicationBufferSize
  private val packetSize = engine.getSession.getPacketBufferSize


  var clientDecryptedData = ByteBuffer.allocate(appBufferSize)
  var clientEncryptedData = ByteBuffer.allocate(packetSize)

  var serverDecryptedData = ByteBuffer.allocate(appBufferSize)
  var serverEncryptedData = ByteBuffer.allocate(packetSize)


  def terminate(): Unit = {
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
          engine.closeInbound()
          engine.closeOutbound()
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

  def readChunk(implicit ctx: ExecutionContext) {
    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      readBuf(client, buf => {
        protocol(buf) {
          (resp, rid) =>
            send(Some(ResponseContinuationState(resp, rid)))
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
        engine.closeOutbound()
        engine.closeInbound()
        terminate()
        res
      case r => throw new Exception(s"Cannot send data $r")
    }
  }

  private def handleResponseSent() {
    if (!protocol.keepConnection) {
      terminate()
    } else {
      unSelectForWrite(key)
    }
  }

  def continueWriting() {
    send(writeState)
  }

  def send(state: Option[ResponseContinuationState]) {
    state foreach { st =>
      Try {

        lazy val cont: Iteratee[ByteBuffer, Option[ResponseContinuationState]] = Cont {
          case Data(d) =>
            keyLog(key, "Sending buffer " + System.identityHashCode(d) + " " + d)
            val client = key.channel().asInstanceOf[SocketChannel]
            drain(client, d) match {
              case (0, buf) =>
                keyLog(key, " Socket full " + System.identityHashCode(d))
                Done(state, Empty)
              case (_, buf) if !buf.hasRemaining || d.hasRemaining =>
                cont
              case (-1, _) =>
                net.shift.io.Error[ByteBuffer, Option[ResponseContinuationState]](new IOException("Client connection closed."))
            }
          case EOF =>
            Done(None, EOF)
        }

        val res = st.content(cont)
        keyLog(key, " res " + res)

        res match {
          case Done(s, Empty) =>
            keyLog(key, s.map {
              _.requestId
            } + " response: continue sending")
            writeState = s
            selectForWrite(key)
            ()
          case Done(_, EOF) =>
            handleResponseSent()
          case Error(t) =>
            log.error("Cannot sent response ", t)
            terminate()
          case it =>
            log.error("Unexpected iteratee " + it)
            terminate()
        }

      }.recover {
        case e: ClosedChannelException =>
          log.warn("Client closed the connection while writing.")
          terminate()
        case e: Exception =>
          log.error("Internal error ", e)
          terminate()
      }
    }
  }

}

