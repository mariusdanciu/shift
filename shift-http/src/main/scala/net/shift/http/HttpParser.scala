package net.shift.http

import scala.util.Success
import scala.util.Try
import net.shift.common.BinReader
import net.shift.common.ShiftParsers
import net.shift.io.IO
import java.nio.ByteBuffer

object HttpParser extends App {

  val http = "GET /product?id=123123 HTTP/1.1\r\nContent-Length: 13\r\nContent-Type: text/html\r\n\r\n" + """<html>
<head>
  <title>An Example Page</title>
</head>
<body>
  Hello World, this is a very simple HTML document.
</body>
</html>"""

  new HttpParser().parse(http) match {
    case Success(h @ HTTPRequest(_, _, _, _, body)) =>
      println(h)
    case f => println(f)
  }
}

class HttpParser extends ShiftParsers {

  def uri = ws ~> (opt((str("http://") ~> notReserved()) ~ opt(chr(':') ~> int)) ~ opt(notReserved('/')) ~ (ws ~> opt(params))) ^^ {
    case Some(host ~ port) ~ path ~ params => HTTPUri(Some(host), port, path getOrElse "/", params getOrElse Nil)
    case None ~ path ~ params              => HTTPUri(None, None, path getOrElse "/", params getOrElse Nil)
  }

  def params: Parser[List[HTTPParam]] = chr('?') ~>
    repsep((notReserved() <~ chr('=')) ~ repsep(notReserved(), chr(',')), chr(';')) ^^ {
      _ map {
        case name ~ value => HTTPParam(name, value)
      }
    }

  def httpLine = capitals ~ uri ~ (str("HTTP/") ~> digit) ~ (chr('.') ~> digit) ^^ {
    case method ~ uri ~ major ~ minor =>
      (method,
        uri,
        HTTPVer(major, minor))
  }

  def httpHeaders: Parser[Seq[HeaderItem]] = rep((notReserved() <~ chr(':')) ~ until(crlf, false)) ^^ {
    _ flatMap {
      case "Cookie" ~ value =>
        Cookie.fromString(IO.bufferToString(value))
      case name ~ value => List(TextHeader(name, IO.bufferToString(value)))
    }
  }

  def httpBody = until(atEnd, false) ^^ { a => HTTPBody(List(a)) }

  def http = httpLine ~ (crlf ~> httpHeaders) ~ (crlf ~> httpBody) ^^ {
    case (method, uri, ver) ~ headers ~ body => HTTPRequest(method, uri, ver, headers, body)
  }

  def parse(reader: BinReader): Try[HTTPRequest] = {
    http(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, _) => scala.util.Failure(new Exception(f))
      case Error(f, _)   => scala.util.Failure(new Exception(f))
    }
  }

  def parse(html: String): Try[HTTPRequest] = parse(BinReader(List(ByteBuffer.wrap(html.getBytes("UTF-8")))))
}

