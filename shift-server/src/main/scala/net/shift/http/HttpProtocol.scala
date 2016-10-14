package net.shift.http

import net.shift.http._
import net.shift.common.BinReader
import scala.concurrent.ExecutionContext
import net.shift.protocol.Protocol
import net.shift.io.BinProducer
import java.nio.ByteBuffer
import scala.util.Success
import scala.concurrent.Future
import net.shift.server.RawExtract
import net.shift.common.Path
import scala.util.Try

object HttpProtocol {
  def apply(service: HTTPService) = new HttpProtocol(service)

  def services(list: Service*) = {

  }
}

trait Service {

}

case class Get(f: PartialFunction[(Path, Request, AsyncResponse), Unit]) extends Service {
  def run(req: Request, resp: AsyncResponse): Boolean = {
    val arg = (Path(req.uri.path), req, resp)
    if (f.isDefinedAt(arg)) {
      f(arg)
      true
    } else {
      false
    }
  }
}

object AsInt {
  def unapply(s: String): Option[Int] = Try {
    s.toInt
  }.toOption
}

object Scheme {
  def unapply(p: Path): Option[List[String]] = {
    Some(p.parts)
  }
}

case class /:(first: String, rest: Path) extends Path {
  def scheme: Option[String] = None
  def parts: List[String] = List(first) ++ rest.parts
  def +(part: String): Path = new /:(first, rest + part)
  def ++(p: Path): Path = new /:(first, rest ++ p)

  def /:(s: String): /: = new /:(first, rest + s)
  def /:(concat: Path): /: = new /:(first, rest ++ concat)
}

case object EP extends Path {
  def scheme: Option[String] = None
  def parts: List[String] = Nil
  def +(part: String): Path = new /:(part, EP)
  def ++(p: Path): Path = p

  def /:(s: String): Path = new /:(s, EP)
  def /:(concat: Path): Path = concat
}

class HttpProtocol(service: HTTPService) extends Protocol {
  val log = HttpLog
  var keepAlive = true
  var readState: Option[Payload] = None

  def keepConnection = keepAlive

  def apply(in: ByteBuffer)(write: (BinProducer, String) => Unit)(implicit ctx: ExecutionContext) {

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