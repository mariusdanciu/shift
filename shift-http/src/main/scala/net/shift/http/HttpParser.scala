package net.shift.http

import scala.util.Success
import scala.util.Try
import net.shift.common.BinReader
import net.shift.common.ShiftParsers
import net.shift.io.IO
import java.nio.ByteBuffer
import java.net.URLDecoder
import net.shift.common.Log
import org.apache.log4j.Logger

object HTTPLog extends Log {
  def loggerName = "http_server"
}

class HttpParser extends ShiftParsers {

  val log = HTTPLog

  def uri = ws ~> (opt((str("http://") ~> notReserved()) ~ opt(chr(':') ~> int)) ~ opt(path) ~ (ws ~> opt(chr('?') ~> params))) ^^ {
    case Some(host ~ port) ~ path ~ params => HTTPUri(Some(host), port, path getOrElse "/", params getOrElse Nil)
    case None ~ path ~ params              => HTTPUri(None, None, path getOrElse "/", params getOrElse Nil)
  }

  def params: Parser[List[HTTPParam]] = repsep(notReserved() ~ opt(chr('=') ~> repsep(notReserved(), chr(','))), chr('&')) ^^ {
    _ map {
      case name ~ Some(value) => HTTPParam(URLDecoder.decode(name, "UTF-8"), value map { URLDecoder.decode(_, "UTF-8") })
      case name ~ _           => HTTPParam(URLDecoder.decode(name, "UTF-8"), Nil)
    }
  }

  def httpLine = capitals ~ uri ~ (str("HTTP/") ~> digit) ~ (chr('.') ~> digit) <~ crlf ^^ {
    case method ~ uri ~ major ~ minor =>
      (method,
        uri,
        HTTPVer(major, minor))
  }

  def cookie: Parser[List[Cookie]] = (str("Cookie") <~ chr(':') <~ ws) ~> repsep((ws ~> notReserved() <~ ws <~ chr('=') <~ ws) ~ notReserved('='), chr(';')) <~ crlf ^^ {
    _ map {
      case k ~ v => Cookie(k, v)
    }
  }

  def header: Parser[List[TextHeader]] = ((notReserved() <~ chr(':') <~ ws) ~ until(crlf, false)) ^^ {
    case name ~ value =>
      List(TextHeader(name.trim, IO.bufferToString(value)))
  }

  def httpHeaders: Parser[Seq[HeaderItem]] = rep(cookie | header) ^^ { _ flatten }

  def httpBody = until(atEnd, false) ^^ { a =>
    HTTPBody(List(a))
  }

  def http = httpLine ~ httpHeaders ~ (crlf ~> opt(httpBody)) ^^ {
    case (method, uri, ver) ~ headers ~ body =>
      HTTPRequest(method, uri, ver, headers, body getOrElse HTTPBody.empty)
  }

  def parse(reader: BinReader): Try[HTTPRequest] = {
    if (log.isInfo) {
      log.info(IO.buffersToString(reader.in))
      reader.in map { _ flip }
    }

    http(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, p) =>
        log.info("Failed at position: " + p.pos.column)
        scala.util.Failure(new Exception(f))
      case Error(f, p) =>
        log.info("Error at position: " + p.pos.column)
        scala.util.Failure(new Exception(f))
    }
  }

  def parseParams(p: String) = params(BinReader(List(ByteBuffer.wrap(p.getBytes("UTF-8"))))) match {
    case Success(r, _) => scala.util.Success(r)
    case Failure(f, p) =>
      scala.util.Failure(new Exception(f))
    case Error(f, p) =>
      scala.util.Failure(new Exception(f))
  }

  def parse(http: String): Try[HTTPRequest] = parse(BinReader(List(ByteBuffer.wrap(http.getBytes("UTF-8")))))
}

