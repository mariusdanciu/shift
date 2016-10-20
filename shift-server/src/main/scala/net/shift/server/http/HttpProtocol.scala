package net.shift.server.http

import net.shift.server.http._
import net.shift.common.BinReader
import scala.concurrent.ExecutionContext
import net.shift.server.protocol.Protocol
import scala.util.Success
import scala.concurrent.Future
import net.shift.server.RawExtract
import net.shift.io.BinProducer
import java.nio.ByteBuffer
import net.shift.server.protocol.ProtocolBuilder

object HttpProtocolBuilder {
  def apply(service: HTTPService) = new HttpProtocolBuilder(service)
}

class HttpProtocolBuilder(service: HTTPService) extends ProtocolBuilder {
  def createProtocol = new HttpProtocol(service)
}

class HttpProtocol(service: HTTPService) extends Protocol {
  val log = HttpLog
  var keepAlive = true
  var readState: Option[Payload] = None

  def createNew = new HttpProtocol(service)

  def keepConnection = keepAlive

  def apply(in: ByteBuffer)(write: (BinProducer, String) => Unit)(implicit ctx: ExecutionContext) {

    readState match {
      case RawExtract(raw) =>
        val msg = raw + in

        new HttpParser().parse(BinReader(msg.duplicates)) match {
          case r @ Success(req) =>
            keepAlive = req.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
            tryRun(msg.size, req, write)
          case r =>
            readState = Some(msg)
        }

      case Some(h @ Request(m, u, v, hd, body @ Body(seq))) =>
        val newSize = body.size + in.limit()
        val msg = Body(seq ++ Seq(in))
        val req = h.copy(body = msg)
        tryRun(newSize, req, write)
    }
  }

  private def tryRun(size: Long, req: Request, write: (BinProducer, String) => Unit)(implicit ctx: ExecutionContext) {
    if (requestComplete(size, req)) {
      readState = None
      Future {
        log.info("Processing " + req)
        service(req)(resp => {
          write(resp.asBinProducer, req.uri.toString)
        })
      }
    } else {
      readState = Some(req)
    }
  }

  private def requestComplete(bodySize: Long, http: Request): Boolean = {
    val contentLength = http.longHeader("Content-Length").getOrElse(-1L)
    contentLength <= bodySize
  }

}