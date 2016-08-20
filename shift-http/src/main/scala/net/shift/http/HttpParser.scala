package net.shift.http

import scala.util.parsing.combinator.Parsers
import net.shift.common.ShiftParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.Try

object HttpParser extends App {

  println(new HttpParser().parse("GET /where?q=now:8080 HTTP/1.1"))
}

class HttpParser extends ShiftParsers {

  def uri = opt(acceptSeq("http://")) ~> uriValid

  def httpLine = capitals ~ (ws ~> uri) ~ opt(accept(':') ~> int) ~ (ws ~> acceptSeq("HTTP/") ~> digit) ~ (accept('.') ~> digit) ^^ {
    case method ~ uri ~ port ~ major ~ minor => HTTP(method, uri, port getOrElse 80, s"$major.$minor")
  }

  def parse(html: String): Try[HTTP] = {
    httpLine(new CharSequenceReader(html)) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, _) => scala.util.Failure(new Exception(f))
      case Error(f, _)   => scala.util.Failure(new Exception(f))
    }
  }
}

case class HTTP(method: String, uri: String, port: Int, version: String)
case class Params(params: List[(String, List[String])])
