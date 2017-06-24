package net.shift.server.http

import java.nio.ByteBuffer

import net.shift.common.{BinReader, LogBuilder}
import net.shift.io.BinProducer
import net.shift.server.protocol.{Protocol, ProtocolBuilder}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

object HttpProtocolBuilder {
  def apply(service: HTTPService) = new HttpProtocolBuilder(service)
}

class HttpProtocolBuilder(service: HTTPService) extends ProtocolBuilder {
  def createProtocol = new HttpProtocol(service)
}

class HttpProtocol(service: HTTPService) extends Protocol {
  private val log = LogBuilder.logger(classOf[HttpProtocol])

  var keepAlive = true
  var readState: Option[Payload] = None

  def createNew = new HttpProtocol(service)

  def keepConnection: Boolean = keepAlive

  def apply(in: ByteBuffer)(write: BinProducer => Unit)(implicit ctx: ExecutionContext) {

    readState match {
      case RawExtract(raw) =>
        val msg = raw + in

        new HttpParser().parse(BinReader(msg.duplicates)) match {
          case r@Success(req) =>
            keepAlive = req.stringHeader("Connection").forall(_ == "keep-alive")
            tryRun(msg.size, req, write)
          case _ =>
            readState = Some(msg)
        }

      case Some(h@Request(m, u, v, hd, body@Body(seq))) =>
        val newSize = body.size + in.limit()
        val msg = Body(seq ++ Seq(in))
        val req = h.copy(body = msg)
        tryRun(newSize, req, write)
    }
  }

  private def tryRun(size: Long, req: Request, write: BinProducer => Unit)(implicit ctx: ExecutionContext) {
    if (requestComplete(size, req)) {
      readState = None
      Future {
        log.info("Processing " + req)
        service(req)(resp => {
          log.info("Got response")
          write(resp.asBinProducer)
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

object RawExtract {
  def unapply(t: Option[Payload]): Option[Raw] = t match {
    case None => Some(Raw(Nil))
    case Some(raw: Raw) => Some(raw)
    case _ => None
  }
}

case class Raw(buffers: List[ByteBuffer]) extends Payload {
  def +(b: ByteBuffer) = Raw(buffers ++ List(b))

  def ++(b: Seq[ByteBuffer]) = Raw(buffers ++ b)

  def size: Int = buffers map {
    _.limit
  } sum

  def buffersState: String = buffers map { b => s"${b.position} : ${b.limit}" } mkString "\n"

  def duplicates: List[ByteBuffer] = buffers map {
    _ duplicate
  }
}