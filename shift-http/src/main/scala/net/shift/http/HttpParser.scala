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
      println(new String(body.body, "UTF-8"))
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
        port getOrElse 80,
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

  def parse(html: String): Try[HTTP] = {
    http(BinReader(html.getBytes("UTF-8"))) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, _) => scala.util.Failure(new Exception(f))
      case Error(f, _)   => scala.util.Failure(new Exception(f))
    }
  }
}

case class HTTP(line: HTTPLine, headers: List[HTTPHeader], body: HTTPBody)
case class HTTPLine(method: String,
                    uri: String,
                    port: Int,
                    version: HTTPVer,
                    params: List[HTTPParam])

case class HTTPParam(name: String, value: List[String])
case class HTTPVer(major: Byte, minor: Byte)
case class HTTPHeader(name: String, value: String)
case class HTTPBody(body: Array[Byte])
