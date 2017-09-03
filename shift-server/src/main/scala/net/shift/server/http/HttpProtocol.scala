package net.shift.server.http

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicBoolean

import net.shift.common.{BinReader, LogBuilder}
import net.shift.io.{BinProducer, IO}
import net.shift.server.protocol.{Protocol, ProtocolBuilder}
import net.shift.server.{AlreadyCommitted, CommitFailure, Committed, ResponseFeedback}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

case class HttpProtocolBuilder(service: HttpService) extends ProtocolBuilder {
  def createProtocol = HttpProtocol(service)
}

case class HttpProtocol(service: HttpService) extends Protocol {
  private val log = LogBuilder.logger(classOf[HttpProtocol])

  var keepAlive = true
  var readState: Option[Payload] = None

  def createNew = HttpProtocol(service)

  def keepConnection: Boolean = keepAlive

  def apply(in: ByteBuffer)(write: BinProducer => Unit)(implicit ctx: ExecutionContext) {

    readState match {
      case RawExtract(raw) =>
        val msg = raw + in

        val toParse = BinReader(msg.duplicates)
        new HttpParser().parse(toParse) match {
          case r@Success(req) =>
            keepAlive = req.stringHeader("Connection").forall(_ == "keep-alive")

            IO.sizeOf(req.body).map {
              tryRun(_, req, write)
            }
          case Failure(f) =>
            log.warn("Failed parsing buffer " + f + " " + toParse)
            readState = Some(msg)
        }

      case Some(h@Request(_, _, _, _, body@Body(seq))) =>
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
        log.debug("Processing " + req)
        service(req)(new CallOnceResponseFunction(write))
      }
    } else {
      readState = Some(req)
    }
  }

  private def requestComplete(bodySize: Long, http: Request): Boolean = {
    val contentLength = http.longHeader("Content-Length").getOrElse(-1L)
    log.debug(s"request complete: Content-Length: $contentLength : body size: $bodySize - ${contentLength <= bodySize}")
    contentLength <= bodySize
  }


  class CallOnceResponseFunction(write: BinProducer => Unit) extends (Response => ResponseFeedback) {

    val flag = new AtomicBoolean(false)

    override def apply(resp: Response): ResponseFeedback = {
      try {
        if (flag.compareAndSet(false, true)) {
          log.debug("Got response")
          write(resp.asBinProducer)
          Committed
        } else {
          AlreadyCommitted
        }
      } catch {
        case t: Throwable => CommitFailure(t)
      }
    }
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

