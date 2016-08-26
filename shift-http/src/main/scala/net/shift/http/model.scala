package net.shift.http

import net.shift.io._
import java.nio.ByteBuffer

trait Payload

case class HTTPParam(name: String, value: List[String])
case class HTTPVer(major: Byte, minor: Byte)

trait HeaderItem {
  def name: String
  def raw: String
}

case class TextHeader(name: String, value: String) extends HeaderItem {
  def raw = s"$name : $value"
}

object Cookie {
  def apply(name: String, value: String) = new Cookie(name, value, None, None, None, None, false, false)
  def apply(name: String, value: String, path: String) = new Cookie(name, value, None, Some(path), None, None, false, false)

  def fromString(str: String): Seq[Cookie] = {
    ((Nil: List[Cookie]) /: str.split(";"))(
      (acc, e) => e.split("=").toList match {
        case value :: Nil       => acc ++ List(Cookie("", value))
        case name :: value :: _ => acc ++ List(Cookie(name, value))
        case _                  => acc
      })
  }
}

case class Cookie(name: String,
                  value: String,
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Long],
                  version: Option[Int],
                  secure: Boolean,
                  httpOnly: Boolean) extends HeaderItem {

  def setName(value: String) = this.copy(name = value)
  def setValue(value: String) = this.copy(value = value)
  def setDomain(value: String) = this.copy(domain = Some(value))
  def setPath(value: String) = this.copy(path = Some(value))
  def setMaxAge(value: Long) = this.copy(maxAge = Some(value))
  def setVersion(value: Int) = this.copy(version = Some(value))
  def setSecure(value: Boolean) = this.copy(secure = value)
  def setHttpOnly(value: Boolean) = this.copy(httpOnly = value)

  def raw: String = s"${name}=${value}" +
    domain.map(d => s";Domain=$d").getOrElse("") +
    maxAge.map(d => s";Max-Age=$d").getOrElse("") +
    path.map(d => s";Path=$d").getOrElse("") +
    (if (secure) ";Secure" else "") +
    version.map(d => s";Version=$d").getOrElse("")
}

object HTTPBody {

  def apply(body: String) = new HTTPBody(List(ByteBuffer.wrap(body.getBytes("UTF-8"))))
}
case class HTTPBody(parts: Seq[ByteBuffer]) extends BinProducer {
  def size = parts.map { _.limit }.sum

  def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {
    val data = (parts map { d => Data(d) }) ++ List(EOF)
    (it /: data) {
      case (Cont(f), e) => f(e)
      case (r, _)       => r
    }
  }
}

case class HTTPUri(host: Option[String], port: Option[Int], path: String, params: List[HTTPParam])

case class HTTPRequest(
    method: String,
    uri: HTTPUri,
    version: HTTPVer,
    headers: Seq[HeaderItem],
    body: HTTPBody) extends Payload {

  def header(name: String): Option[HeaderItem] = headers find { _.name == name }

  def contentLength = header("Content-Length").map {
    case TextHeader(name, value) => value.trim.toInt
  } getOrElse -1

  lazy val cookies = headers flatMap {
    case c: Cookie => List(c)
    case _         => Nil
  }

  def cookie(name: String) = cookies find (_.name == name)
}

case class HTTPResponse(code: Int,
                        reason: String = "OK",
                        headers: List[HeaderItem] = Nil,
                        body: BinProducer) extends BinProducer with Payload {

  def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {

    val headersStr = headers map {
      case h => s"${h.raw}\r\n"
    } mkString

    val heads = Data(ByteBuffer.wrap(s"HTTP/1.1 $code $reason\r\n$headersStr\r\n".getBytes("UTF-8")))
    val next = it match {
      case Cont(f) => f(heads)
      case r       => r
    }
    body(next)

  }
}

