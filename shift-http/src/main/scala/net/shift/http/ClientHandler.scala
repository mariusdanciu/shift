package net.shift.http

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.SocketChannel

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import Selections._
import net.shift.common.BinReader
import net.shift.common.Log
import net.shift.io.Iteratee
import net.shift.io._
import net.shift.io.IO._

class ClientHandler(key: SelectionKey, name: String) extends Log with KeyOps {

  var readState: Option[Payload] = None
  var writeState: Option[BinProducer] = None
  var keepAlive: Boolean = true

  def loggerName = name

  def readChunk(service: HTTPService)(implicit ctx: ExecutionContext) = {

    def tryParse(msg: Raw): Option[HTTPRequest] = {
      BinReader(IO.fromChunks(msg.buffers)).toOption.flatMap { reader =>
        new HttpParser().parse(reader) match {
          case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
            Some(h)
          case Failure(f) =>
            log.debug("Cannot parser HTTP request ", f)
            None
        }
      }
    }

    def checkRequestComplete(bodySize: Long, contentLength: Option[Long], http: HTTPRequest) = {
      if (contentLength.getOrElse(-1L) > bodySize) {
        readState = Some(http)
      } else {
        keepAlive = http.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
        readState = None

        Future {
          service(http, resp => {
            send(IO.segmentable(resp.asBinProducer))
          })
        }
      }
    }

    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      val buf = ByteBuffer.allocate(1024)
      var size = client.read(buf)

      if (size > 0) {
        buf.flip()
        readState match {
          case RawExtract(raw) =>
            val msg = raw + buf
            tryParse(msg) match {
              case Some(http @ HTTPRequest(m, u, v, hd, body @ HTTPBody(seq))) =>
                checkRequestComplete(body.size, http.longHeader("Content-Length"), http)
              case None =>
                readState = Some(msg)
              case _ =>
                log.error("Cannot read request")
                closeClient(key)
            }
          case Some(h @ HTTPRequest(m, u, v, hd, body @ HTTPBody(seq))) =>
            val newSize = body.size + size
            val cl = h.longHeader("Content-Length")
            val msg = HTTPBody(seq ++ Seq(buf))
            val req = HTTPRequest(m, u, v, hd, msg)
            checkRequestComplete(newSize, cl, req)
        }

      } else if (size < 0) {
        log.info("End of client stream")
        closeClient(key)
      }
    }.recover {
      case e =>
        log.error("Cannot read data from client: ", e)
        closeClient(key)

    }
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    var written = client.write(buffer)
    while (written > 0 && buffer.hasRemaining()) {
      written = client.write(buffer)
    }
    (written, buffer)
  }

  private def handleResponseSent() = {
    writeState = None
    if (!keepAlive) {
      closeClient(key)
    } else {
      unSelectForWrite(key)
    }
  }

  def writeResponse() = {
    writeState match {
      case Some(prod) =>
        val client = key.channel().asInstanceOf[SocketChannel]
        send(prod)
      case _ =>
        log.warn("No data available for write")
    }

  }
  private def send(prod: BinProducer) {
    Try {
      lazy val cont: Iteratee[ByteBuffer, Unit] = Cont {
        case Data(d) =>
          val client = key.channel().asInstanceOf[SocketChannel]
          drain(client, d) match {
            case (0, buf) =>
              writeState = Some(prod)
              selectForWrite(key)
              Done((), Data(d))
            case (-1, buf) => net.shift.io.Error[ByteBuffer, Unit](new IOException("Client connection closed."))
            case (_, buf)  => cont
          }
        case EOF =>
          handleResponseSent()
          Done((), EOF)
      }

      prod(cont) match {
        case Done(_, EOF) =>
          handleResponseSent()
        case Error(t) =>
          log.error("Cannot sent response ", t)
          closeClient(key)
        case it =>
          log.error("Unexpected iteratee " + it)
          closeClient(key)
      }

    }.recover {
      case e: Exception =>
        log.error("Internal error ", e)
        closeClient(key)
    }
  }

}