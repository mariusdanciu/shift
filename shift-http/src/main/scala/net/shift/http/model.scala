package net.shift.http

sealed trait HTTPEntity

case class HTTPParam(name: String, value: List[String]) extends HTTPEntity
case class HTTPVer(major: Byte, minor: Byte) extends HTTPEntity
case class HTTPHeader(name: String, value: String) extends HTTPEntity
case class HTTPBody(parts: Seq[Array[Byte]]) extends HTTPEntity {
  def size = parts.map { _.length }.sum
  def compact = (new Array[Byte](0) /: parts) { (acc, a) => acc ++ a }
}
case class HTTPUri(host: Option[String], port: Option[Int], path: String, params: List[HTTPParam]) extends HTTPEntity

case class HTTPRequest(
    method: String,
    uri: HTTPUri,
    version: HTTPVer,
    headers: List[HTTPHeader],
    body: HTTPBody) extends HTTPEntity {

  def header(name: String): Option[HTTPHeader] = headers find { _.name == name }

  def contentLength = header("Content-Length").map { _.value.trim.toInt } getOrElse -1

}
