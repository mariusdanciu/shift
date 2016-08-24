package net.shift.http

import net.shift.io._

case class HTTPParam(name: String, value: List[String])
case class HTTPVer(major: Byte, minor: Byte)
case class HTTPHeader(name: String, value: String)

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
                        reason: String,
                        cookies: List[Cookie],
                        headers: List[HTTPHeader],
                        body: HTTPBody)

