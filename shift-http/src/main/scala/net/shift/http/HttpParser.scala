package net.shift.http

import scala.util.parsing.combinator.Parsers
import net.shift.common.ShiftParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.Try
import net.shift.common.BinReader
import scala.util.Success

object HttpParser extends App {

  val http = "GET /where?q=now,is,here:8080 HTTP/1.1\r\nContent-Length: 13\r\nContent-Type: text/html\r\n\r\n" + """<html>
<head>
  <title>An Example Page</title>
</head>
<body>
  Hello World, this is a very simple HTML document.
</body>
</html>"""

  new HttpParser().parse(http) match {
    case Success(h @ HTTP(_, _, body)) =>
      println(h)
      println(new String(body.message, "UTF-8"))
    case f => println(f)
  }
}

class HttpParser extends ShiftParsers {

  def uri = opt(str("http://")) ~> (ws ~> uriValid)

  def params: Parser[List[HTTPParam]] = chr('?') ~>
    repsep((notReserved <~ chr('=')) ~ repsep(notReserved, chr(',')), chr(';')) ^^ {
      _ map {
        case name ~ value => HTTPParam(name, value)
      }
    }

  def httpLine = capitals ~ uri ~ opt(params) ~ opt(chr(':') ~> int) ~ (str("HTTP/") ~> digit) ~ (chr('.') ~> digit) ^^ {
    case method ~ uri ~ params ~ port ~ major ~ minor =>
      HTTPLine(method,
        uri,
        port,
        HTTPVer(major, minor),
        params getOrElse Nil)
  }

  def httpHeaders: Parser[List[HTTPHeader]] = rep((notReserved <~ chr(':')) ~ until(crlf, false)) ^^ {
    _ map {
      case name ~ value => HTTPHeader(name, new String(value, "UTF-8"))
    }
  }

  def httpBody = until(atEnd, false) ^^ { HTTPBody }

  def http = httpLine ~ (crlf ~> httpHeaders) ~ (crlf ~> httpBody) ^^ {
    case line ~ headers ~ body => HTTP(line, headers, body)
  }

  def parse(reader: BinReader): Try[HTTP] = {
    http(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, _) => scala.util.Failure(new Exception(f))
      case Error(f, _)   => scala.util.Failure(new Exception(f))
    }
  }

  def parse(html: String): Try[HTTP] = parse(BinReader(html.getBytes("UTF-8")))
}

case class HTTP(line: HTTPLine, headers: List[HTTPHeader], body: HTTPBody) {
  def header(name: String): Option[HTTPHeader] = headers find { _.name == name }
}

case class HTTPLine(method: String,
                    uri: String,
                    port: Option[Int],
                    version: HTTPVer,
                    params: List[HTTPParam])

case class HTTPParam(name: String, value: List[String])
case class HTTPVer(major: Byte, minor: Byte)
case class HTTPHeader(name: String, value: String)
case class HTTPBody(message: Array[Byte])
