package net.shift.server.http

import java.nio.ByteBuffer

import scala.util.Try

import net.shift.io.BinProducer
import net.shift.io.Cont
import net.shift.io.Data
import net.shift.io.Done
import net.shift.io.EOF
import net.shift.io.IO
import net.shift.io.Iteratee
import net.shift.loc.Language

trait Payload

case class Param(name: String, value: List[String])

object Ver {
  val Ver_1_1 = Ver(1 toByte, 1 toByte)
}
case class Ver(major: Byte, minor: Byte)

sealed trait HeaderItem {
  def name: String
  def headerLine: String
}

case class TextHeader(name: String, value: String) extends HeaderItem {
  def headerLine = s"$name: $value"
}

case class Cookie(cookieName: String, cookieValue: String) extends HeaderItem {
  def name = "Cookie"
  def headerLine: String = s"$name: $cookieName=cookieValue"
}

object SetCookie {
  def apply(name: String, value: String) = new SetCookie(name, value, None, None, None, None, false, false)
  def apply(name: String, value: String, path: String) = new SetCookie(name, value, None, Some(path), None, None, false, false)
}
case class SetCookie(cookieName: String,
                     cookieValue: String,
                     domain: Option[String],
                     path: Option[String],
                     maxAge: Option[Long],
                     version: Option[Int],
                     secure: Boolean,
                     httpOnly: Boolean) extends HeaderItem {

  def name = "Set-Cookie"

  def setCookieName(value: String): SetCookie = this.copy(cookieName = value)
  def setCookieValue(value: String): SetCookie = this.copy(cookieValue = value)
  def setDomain(value: String): SetCookie = this.copy(domain = Some(value))
  def setPath(value: String): SetCookie = this.copy(path = Some(value))
  def setMaxAge(value: Long): SetCookie = this.copy(maxAge = Some(value))
  def setVersion(value: Int): SetCookie = this.copy(version = Some(value))
  def setSecure(value: Boolean): SetCookie = this.copy(secure = value)
  def setHttpOnly(value: Boolean): SetCookie = this.copy(httpOnly = value)

  def headerLine: String = s"$name: $cookieName=$cookieValue" +
    domain.map(d => s";Domain=$d").getOrElse("") +
    maxAge.map(d => s";Max-Age=$d").getOrElse("") +
    path.map(d => s";Path=$d").getOrElse("") +
    (if (secure) ";Secure" else "") +
    version.map(d => s";Version=$d").getOrElse("")
}

object Body {
  def apply(body: String) = new Body(List(ByteBuffer.wrap(body.getBytes("UTF-8"))))
  def empty = Body(Nil)
}
case class Body(parts: Seq[ByteBuffer]) extends BinProducer {
  def size: Int = parts.map { _.limit }.sum

  def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {
    val data = (parts map { d => Data(d) }) ++ List(EOF)
    (it /: data) {
      case (Cont(f), e) => f(e)
      case (r, _)       => r
    }
  }
}

object Uri {
  def apply(path: String) = new Uri(None, None, path, Nil)
}
case class Uri(host: Option[String], port: Option[Int], path: String, params: List[Param]) {
  def param(name: String): Option[Param] = params find { _.name == name }
  def paramValue(name: String): Option[List[String]] = param(name) map { _.value }
}

case class Request(
    method: String,
    uri: Uri,
    version: Ver,
    headers: Seq[HeaderItem],
    body: BinProducer) extends Payload {

  lazy val languages: Seq[Language] = {
    header("Accept-Language") match {
      case Some(AcceptLanguage(langs)) => langs
      case _                           => List(Language("en"))
    }
  }

  lazy val language: Language = languages head

  def header(name: String): Option[HeaderItem] = headers find { _.name == name }

  def stringHeader(name: String): Option[String] = header(name) match {
    case Some(TextHeader(_, value)) => Some(value.trim)
    case _                             => None
  }

  def longHeader(name: String): Option[Long] = header(name) match {
    case Some(TextHeader(_, value)) => Try(value.trim.toLong).toOption
    case _                             => None
  }

  def intHeader(name: String): Option[Int] = header(name) match {
    case Some(TextHeader(_, value)) => Try(value.trim.toInt).toOption
    case _                             => None
  }

  def booleanHeader(name: String): Option[Boolean] = header(name) match {
    case Some(TextHeader(_, value)) => Try(value.trim.toBoolean).toOption
    case _                             => None
  }

  def doubleHeader(name: String): Option[Double] = header(name) match {
    case Some(TextHeader(_, value)) => Try(value.trim.toDouble).toOption
    case _                             => None
  }

  lazy val cookies: Seq[Cookie] = headers flatMap {
    case c: Cookie => List(c)
    case _         => Nil
  }

  def cookie(name: String): Option[Cookie] = cookies find (_.cookieName == name)
}

case class Response(code: Int,
                    reason: String = "OK",
                    headers: List[HeaderItem] = Nil,
                    body: BinProducer) extends Payload {

  lazy val cookies: List[SetCookie] = headers flatMap {
    case c: SetCookie => List(c)
    case _            => Nil
  }

  def cookie(name: String): Option[SetCookie] = cookies find (_.cookieName == name)

  def asBinProducer: BinProducer = new BinProducer {
    lazy val headerBuffer: BinProducer = {

      val extra = if (body == Body.empty) {
        List(TextHeader("Content-Length", "0"))
      } else
        Nil

      val headersStr = (headers ++ extra) map {
        h => s"${h.headerLine}\r\n"
      } mkString

      val header = s"HTTP/1.1 $code $reason\r\n$headersStr\r\n"

      IO.bufferProducer(ByteBuffer.wrap(header.getBytes("UTF-8")))
    }

    var current: BinProducer = headerBuffer

    def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {

      it match {
        case i if current == headerBuffer =>
          current(i) match {
            case Done(_, EOF) =>
              current = body
              current(it)
            case r => r
          }
        case r => current(r)
      }

    }
  }
}

