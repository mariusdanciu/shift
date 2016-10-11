package net.shift.server

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.SocketChannel
import scala.concurrent.ExecutionContext
import scala.util.Try
import Selections._
import net.shift.io.Iteratee
import net.shift.io._
import net.shift.io.IO._
import java.nio.channels.ClosedChannelException
import net.shift.protocol.Protocol
import net.shift.http.HTTPLog
import net.shift.http.HTTPUri

private[server] class ClientHandler(key: SelectionKey, name: String, onClose: SelectionKey => Unit, protocol: Protocol) {

  val log = HTTPLog

  var writeState: Option[BinProducer] = None

  var uri: HTTPUri = HTTPUri("???")

  def loggerName = name

  def terminate {
    onClose(key)
  }

  def readChunk(implicit ctx: ExecutionContext) = {

    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      val buf = ByteBuffer.allocate(1024)
      var size = client.read(buf)
      log.debug(s"Read $size bytes")

      if (size > 0) {
        buf.flip()
        protocol(buf) { send }
      } else if (size < 0) {
        log.info("End of client stream")
        terminate
      } else if (size == 0) {
        log.info("No data to read")
      }
    }.recover {
      case e =>
        log.error("Cannot read data from client: " + e.getMessage)
        terminate
    }
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    var written = client.write(buffer)
    log.debug(uri + " - response: wrote " + written)
    while (written > 0 && buffer.hasRemaining()) {
      written = client.write(buffer)
      log.debug(uri + " - response: wrote " + written)
    }
    (written, buffer)
  }

  private def handleResponseSent() = {
    writeState = None
    if (!protocol.keepConnection) {
      terminate
    } else {
      unSelectForWrite(key)
    }
  }

  def writeResponse() = {
    writeState match {
      case Some(prod) =>
        log.debug(uri + " response: send " + prod)
        send(prod)
      case _ =>
        log.warn("No data available for write")
    }

  }
  private def send(prod: BinProducer) {
    Try {
      lazy val cont: Iteratee[ByteBuffer, Unit] = Cont {
        case Data(d) =>
          log.debug(uri + " Sending buffer " + System.identityHashCode(d))
          val client = key.channel().asInstanceOf[SocketChannel]
          drain(client, d) match {
            case (0, buf) =>
              log.debug(uri + " Socket full " + System.identityHashCode(d))
              writeState = Some(prod)
              Done((), Data(buf))
            case (_, buf) if (!buf.hasRemaining) => cont
            case (-1, buf)                       => net.shift.io.Error[ByteBuffer, Unit](new IOException("Client connection closed."))
          }
        case EOF =>
          handleResponseSent()
          Done((), EOF)
      }

      val res = prod(cont)
      log.debug(uri + " res " + res)

      res match {
        case Done(_, Data(_)) =>
          log.debug(uri + " response: continue sending")
          selectForWrite(key)
          key.selector().wakeup()
        case Done(_, EOF) =>
          handleResponseSent()
        case Error(t) =>
          log.error("Cannot sent response ", t)
          terminate
        case it =>
          log.error("Unexpected iteratee " + it)
          terminate
      }

    }.recover {
      case e: ClosedChannelException =>
        log.warn("Client closed the connection while writing.")
        terminate
      case e: Exception =>
        log.error("Internal error ", e)
        terminate
    }
  }

}