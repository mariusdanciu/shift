package net.shift.http

import scala.util.Success
import scala.util.Try
import net.shift.common.BinReader
import net.shift.common.ShiftParsers
import net.shift.io.IO
import java.nio.ByteBuffer

class HttpParser extends ShiftParsers {

  def uri = ws ~> (opt((str("http://") ~> notReserved()) ~ opt(chr(':') ~> int)) ~ opt(notReserved('/')) ~ (ws ~> opt(params))) ^^ {
    case Some(host ~ port) ~ path ~ params => HTTPUri(Some(host), port, path getOrElse "/", params getOrElse Nil)
    case None ~ path ~ params              => HTTPUri(None, None, path getOrElse "/", params getOrElse Nil)
  }

  def params: Parser[List[HTTPParam]] = chr('?') ~>
    repsep(notReserved() ~ opt(chr('=') ~> repsep(notReserved(), chr(','))), chr(';')) ^^ {
      _ map {
        case name ~ Some(value) => HTTPParam(name, value)
        case name ~ _           => HTTPParam(name, Nil)
      }
    }

  def httpLine = capitals ~ uri ~ (str("HTTP/") ~> digit) ~ (chr('.') ~> digit) ^^ {
    case method ~ uri ~ major ~ minor =>
      (method,
        uri,
        HTTPVer(major, minor))
  }

  def cookie: Parser[List[HeaderItem]] = (str("Cookie") <~ chr(':') <~ ws) ~> repsep((ws ~> notReserved() <~ ws <~ chr('=') <~ ws) ~ notReserved('='), chr(';')) ^^ {
    _ map {
      case k ~ v => Cookie(k, v)
    }
  }

  def header: Parser[List[HeaderItem]] = ((notReserved() <~ chr(':') <~ ws) ~ until(crlf, false)) ^^ {
    case name ~ value =>
      List(TextHeader(name.trim, IO.bufferToString(value)))
  }

  def httpHeaders: Parser[Seq[HeaderItem]] = rep(cookie | header) ^^ { _ flatten }

  def httpBody = until(atEnd, false) ^^ { a =>
    HTTPBody(List(a))
  }

  def http = httpLine ~ (crlf ~> httpHeaders) ~ (crlf ~> opt(httpBody)) ^^ {
    case (method, uri, ver) ~ headers ~ body => HTTPRequest(method, uri, ver, headers, body getOrElse HTTPBody.empty)
  }

  def parse(reader: BinReader): Try[HTTPRequest] = {
    http(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, p) =>
        scala.util.Failure(new Exception(f))
      case Error(f, p) =>
        scala.util.Failure(new Exception(f))
    }
  }

  def parse(http: String): Try[HTTPRequest] = parse(BinReader(List(ByteBuffer.wrap(http.getBytes("UTF-8")))))
}

