package net.shift.protocol.http

import net.shift.http._
import net.shift.common.BinReader
import net.shift.http.HTTPBody
import net.shift.http.Payload
import scala.concurrent.ExecutionContext
import net.shift.protocol.Protocol
import net.shift.io.BinProducer
import net.shift.http.HTTPLog
import net.shift.http.HTTPRequest
import java.nio.ByteBuffer
import scala.util.Success
import scala.concurrent.Future

class HttpProtocol(service: HTTPService) extends Protocol[HTTPRequest] {
  val log = HTTPLog
  var keepAlive = true
  var readState: Option[Payload] = None

  def keepConnection = keepAlive

  def apply(in: ByteBuffer)(write: BinProducer => Unit)(implicit ctx: ExecutionContext) {

    readState match {
      case RawExtract(raw) =>
        val msg = raw + in

        new HttpParser().parse(BinReader(msg.buffers)) match {
          case r @ Success(req) =>
            keepAlive = req.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
            tryRun(msg.size, req, write)
          case r =>
            readState = Some(msg)
        }

      case Some(h @ HTTPRequest(m, u, v, hd, body @ HTTPBody(seq))) =>
        val newSize = body.size + in.limit()
        val msg = HTTPBody(seq ++ Seq(in))
        val req = h.copy(body = msg)
        tryRun(newSize, req, write)
    }
  }

  private def tryRun(size: Long, req: HTTPRequest, write: BinProducer => Unit)(implicit ctx: ExecutionContext) {
    if (requestComplete(size, req)) {
      readState = None
      Future {
        log.info("Processing " + req)
        service(req)(resp => {
          write(resp.asBinProducer)
        })
      }
    } else {
      readState = Some(req)
    }
  }

  private def requestComplete(bodySize: Long, http: HTTPRequest): Boolean = {
    val contentLength = http.longHeader("Content-Length").getOrElse(-1L)
    contentLength <= bodySize
  }

}