package net.shift.server

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}
import javax.net.ssl.{SSLEngine, SSLEngineResult}

import net.shift.io.{Iteratee, _}
import net.shift.server.Selections._
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

private[server] class SSLClientHandler(key: SelectionKey,
                                       engine: SSLEngine,
                                       name: String,
                                       onClose: SelectionKey => Unit,
                                       protocol: Protocol,
                                       readBufSize: Int = 1024) extends SSLOps {

  private var writeState: Option[ResponseContinuationState] = None

  private val appBufferSize = engine.getSession.getApplicationBufferSize
  private val packetSize = engine.getSession.getPacketBufferSize


  private var clientDecryptedData = ByteBuffer.allocate(appBufferSize)
  private var clientEncryptedData = ByteBuffer.allocate(packetSize)

  private val serverDecryptedData = ByteBuffer.allocate(appBufferSize)
  private val serverEncryptedData = ByteBuffer.allocate(packetSize)


  def terminate(): Unit = {
    onClose(key)
  }

  private def readBuf(client: SocketChannel, f: ByteBuffer => Unit) {
    clientEncryptedData.clear()
    val size = client.read(clientEncryptedData)
    log.debug(s"Read $size bytes")

    if (size < 0) {
      log.info("End of client stream")
      terminate()
    } else if (size == 0) {
      log.info("No data to read")
    } else {
      clientEncryptedData.flip()
      while (clientEncryptedData.hasRemaining) {
        clientDecryptedData.clear()

        unwrap(engine, clientEncryptedData, clientDecryptedData) match {
          case OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, src, _) =>
            clientEncryptedData = src
          case OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, _, dest) =>
            clientDecryptedData = dest
          case OPResult(SSLEngineResult.Status.OK, _, dec) =>
            dec.flip()
            f(dec)
          case OPResult(SSLEngineResult.Status.CLOSED, _, _) =>
            engine.closeOutbound()
        }
      }

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

  private def drain(requestId: String, client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    serverEncryptedData.clear()
    wrap(engine, buffer, serverEncryptedData) match {
      case Success(b) =>
        b.flip()
        log.debug("Sending encrypted buf " + b)
        var written = client.write(b)
        log.debug(requestId + " - response: wrote " + written)
        while (written > 0 && b.hasRemaining) {
          written = client.write(b)
          log.debug(requestId + " - response: wrote " + written)
        }
        (written, b)
      case Failure(t) => throw t
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

  private def send(state: Option[ResponseContinuationState]) {
    state foreach { st =>
      Try {

        val rid = st.requestId

        lazy val cont: Iteratee[ByteBuffer, Option[ResponseContinuationState]] = Cont {
          case Data(d) =>
            log.debug(rid + " Sending buffer " + System.identityHashCode(d) + " " + d)
            val client = key.channel().asInstanceOf[SocketChannel]
            drain(rid, client, d) match {
              case (0, buf) =>
                log.debug(rid + " Socket full " + System.identityHashCode(d))
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
        log.debug(st.requestId + " res " + res)

        res match {
          case Done(s, Empty) =>
            log.debug(s.map {
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

