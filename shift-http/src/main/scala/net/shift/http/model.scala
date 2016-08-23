package net.shift.http

sealed trait HTTPEntity

case class HTTPParam(name: String, value: List[String]) extends HTTPEntity
case class HTTPVer(major: Byte, minor: Byte) extends HTTPEntity
case class HTTPHeader(name: String, value: String) extends HTTPEntity
case class HTTPBody(message: Array[Byte]) extends HTTPEntity
case class HTTPUri(host: Option[String], port: Option[Int], path: String, params: List[HTTPParam]) extends HTTPEntity

case class HTTPRequest(
    method: String,
    uri: HTTPUri,
    version: HTTPVer,
    headers: List[HTTPHeader],
    body: HTTPBody) extends HTTPEntity {
  def header(name: String): Option[HTTPHeader] = headers find { _.name == name }
}
