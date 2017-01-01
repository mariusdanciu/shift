package net.shift.server

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}

import net.shift.common.LogBuilder
import net.shift.io.{Iteratee, _}
import net.shift.server.Selections._
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext
import scala.util.Try

private[server] class ClientHandler(key: SelectionKey, name: String, onClose: SelectionKey => Unit, protocol: Protocol, readBufSize: Int = 1024) {

  private val log = LogBuilder.logger(classOf[ClientHandler])

  var writeState: Option[ResponseContinuationState] = None

  def terminate(): Unit = {
    onClose(key)
  }

  private def readBuf(client: SocketChannel, f: ByteBuffer => Unit) {
    val buf = ByteBuffer.allocate(readBufSize)
    var size = client.read(buf)
    log.debug(s"Read $size bytes")

    if (size < 0) {
      log.info("End of client stream")
      terminate()
    } else if (size == 0) {
      log.info("No data to read")
    } else {
      val buffer = if (size <= readBufSize && size > 0) {
        buf.flip()
        val b = ByteBuffer.allocate(buf.limit())
        b.put(buf)
        b.flip
        b
      } else {
        buf
      }
      f(buffer)
    }
  }

  def readChunk(implicit ctx: ExecutionContext) {
    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      readBuf(client, buf => {
        protocol(buf) { (resp, rid) =>
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
    var written = client.write(buffer)
    log.debug(requestId + " - response: wrote " + written)
    while (written > 0 && buffer.hasRemaining) {
      written = client.write(buffer)
      log.debug(requestId + " - response: wrote " + written)
    }
    (written, buffer)
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
            log.debug(rid + " Sending buffer " + System.identityHashCode(d))
            val client = key.channel().asInstanceOf[SocketChannel]
            drain(rid, client, d) match {
              case (0, buf) =>
                log.debug(rid + " Socket full " + System.identityHashCode(d))
                Done(state, Data(buf))
              case (_, buf) if !buf.hasRemaining => cont
              case (-1, _) =>
                net.shift.io.Error[ByteBuffer, Option[ResponseContinuationState]](new IOException("Client connection closed."))
            }
          case EOF =>
            Done(None, EOF)
        }

        val res = st.content(cont)
        log.debug(st.requestId + " res " + res)

        res match {
          case Done(s, Data(_)) =>
            log.debug(s.map { _.requestId } + " response: continue sending")
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

case class ResponseContinuationState(content: BinProducer, requestId: String)