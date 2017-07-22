package net.shift.server.http

import java.net.URLDecoder
import java.nio.ByteBuffer

import scala.util.Try
import net.shift.common.{BinReader, LogBuilder, Path, ShiftParsers}
import net.shift.io.{IO, LocalFileSystem}
import net.shift.io.IO._


class HttpParser extends ShiftParsers {

  private val log = LogBuilder.logger(classOf[HttpParser])

  def uri: Parser[Uri] = ws ~> (opt((str("http://") ~> notReserved()) ~ opt(chr(':') ~> int)) ~ opt(path) ~ (ws ~> opt(chr('?') ~> params))) ^^ {
    case Some(host ~ port) ~ path ~ params => Uri(Some(host), port, URLDecoder.decode(path getOrElse "/", "UTF-8"), params getOrElse Nil)
    case None ~ path ~ params              => Uri(None, None, URLDecoder.decode(path getOrElse "/", "UTF-8"), params getOrElse Nil)
  }

  def params: Parser[List[Param]] = repsep(notReserved() ~ opt(chr('=') ~> repsep(notReserved(), chr(','))), chr('&')) ^^ {
    _ map {
      case name ~ Some(value) => Param(URLDecoder.decode(name, "UTF-8"), value map { URLDecoder.decode(_, "UTF-8") })
      case name ~ _           => Param(URLDecoder.decode(name, "UTF-8"), Nil)
    }
  }

  def httpLine: Parser[(String, Uri, Ver)] = capitals ~ uri ~ (str("HTTP/") ~> digit) ~ (chr('.') ~> digit) <~ crlf ^^ {
    case method ~ uri ~ major ~ minor =>
      (method,
        uri,
        Ver(major, minor))
  }

  def cookie: Parser[List[Cookie]] = (str("Cookie") <~ chr(':') <~ ws) ~> repsep((ws ~> notReserved() <~ ws <~ chr('=') <~ ws) ~ notReserved('='), chr(';')) <~ crlf ^^ {
    _ map {
      case k ~ v => Cookie(k, v)
    }
  }

  def header: Parser[List[TextHeader]] = ((notReserved() <~ chr(':') <~ ws) ~ until(crlf, retryPInput = false)) ^^ {
    case name ~ value =>
      List(TextHeader(name.trim, IO.bufferToString(value)))
  }

  def httpHeaders: Parser[Seq[HeaderItem]] = rep(cookie | header) ^^ { _ flatten }

  def httpBody: Parser[Body] = until(atEnd, retryPInput = false) ^^ { a =>
    Body(List(a))
  }

  def http: Parser[Request] = httpLine ~ httpHeaders ~ (crlf ~> opt(httpBody)) ^^ {
    case (method, uri, ver) ~ headers ~ body =>
      Request(method, uri, ver, headers, body getOrElse Body.empty)
  }

  def parse(reader: BinReader): Try[Request] = {
    if (log.isDebug) {
      val bufs = reader.in.map { _.duplicate }
      log.debug("Parsing data " + (for { b <- bufs } yield {
        b.toString
      }).mkString("\n"))
      log.debug(IO.buffersToString(bufs))
    }

    http(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, p) =>
        log.debug("Failed at position: " + p.pos.column + " " + f)
        scala.util.Failure(new Exception("Failed at position: " + p.pos.column + " " + f))
      case Error(f, p) =>
        log.debug("Error at position: " + p.pos.column + " " + f)
        scala.util.Failure(new Exception("Error at position: " + p.pos.column + " " + f))
    }
  }

  def parseParams(p: String): Try[List[Param]] = params(BinReader(List(ByteBuffer.wrap(p.getBytes("UTF-8"))))) match {
    case Success(r, _) => scala.util.Success(r)
    case Failure(f, _) =>
      scala.util.Failure(new Exception(f))
    case Error(f, _) =>
      scala.util.Failure(new Exception(f))
  }

  def parse(http: String): Try[Request] = parse(BinReader(List(ByteBuffer.wrap(http.getBytes("UTF-8")))))
}

