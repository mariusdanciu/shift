package net.shift.server

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

import net.shift.common.LogBuilder
import net.shift.io.BinProducer
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext

private[server] case class ClientHandler(key: SelectionKey,
                                         onClose: SelectionKey => Unit,
                                         protocol: Protocol,
                                         readBufSize: Int = 1024,
                                         listener: Option[ServerListener]
                                        ) extends ConnectionHandler with KeyLogger {

  protected val log = LogBuilder.logger(classOf[ClientHandler])

  def handleRead()(implicit ec: ExecutionContext): Unit = {
    try {
      val client = key.channel().asInstanceOf[SocketChannel]

      readBuf(client, buf => {
        protocol(buf) { resp =>
          sendResponse(resp)
        }
      })

    } catch {
      case e: Throwable =>
        listener.foreach(_.onError(e))
        log.error("Cannot read data from client: ", e)
        terminate()
    }
  }

  def handleWrite()(implicit ec: ExecutionContext): Unit = {
    try {
      continueSending(drain)
    } catch {
      case t: Throwable => listener.foreach(_.onError(t))
    }
  }

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


  private def drain(client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    var written = client.write(buffer)
    keyLog(key, "Eesponse: wrote " + written)
    while (written > 0 && buffer.hasRemaining) {
      written = client.write(buffer)
      keyLog(key, "Response: wrote " + written)
    }
    (written, buffer)
  }

  def sendResponse(resp: BinProducer) = {
    writeState = Some(ResponseContinuationState(resp))
    continueSending(drain)
  }

}

case class ResponseContinuationState(content: BinProducer)