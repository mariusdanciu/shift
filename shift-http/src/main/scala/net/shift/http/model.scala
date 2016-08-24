package net.shift.http

import net.shift.io._

case class HTTPParam(name: String, value: List[String])
case class HTTPVer(major: Byte, minor: Byte)
case class HTTPHeader(name: String, value: String)

object HTTPBody {
  def apply(body: String) = new HTTPBody(List(body.getBytes("UTF-8")))
}
case class HTTPBody(parts: Seq[Array[Byte]]) extends BinProducer {
  def size = parts.map { _.length }.sum

  def apply[O](it: Iteratee[Array[Byte], O]): Iteratee[Array[Byte], O] = {
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
    headers: List[HTTPHeader],
    body: HTTPBody) {

  def header(name: String): Option[HTTPHeader] = headers find { _.name == name }

  def contentLength = header("Content-Length").map { _.value.trim.toInt } getOrElse -1
}

case class Cookie(name: String,
                  value: String,
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Long],
                  version: Option[Int],
                  secure: Boolean,
                  httpOnly: Boolean)

case class HTTPResponse(code: Int,
                        reason: String = "OK",
                        cookies: List[Cookie] = Nil,
                        headers: List[HTTPHeader] = Nil,
                        body: HTTPBody) extends BinProducer {

  def apply[O](it: Iteratee[Array[Byte], O]): Iteratee[Array[Byte], O] = {

    val headersStr = headers map {
      case HTTPHeader(name, value) => s"$name: $value\r\n"
    } mkString

    val heads = Data(s"HTTP/1.1 $code $reason\r\n$headersStr\r\n".getBytes("UTF-8"))
    val next = it match {
      case Cont(f) => f(heads)
      case r       => r
    }
    body(next)

  }
}

