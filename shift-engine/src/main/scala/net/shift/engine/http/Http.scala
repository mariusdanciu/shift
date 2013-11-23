
package net.shift
package engine
package http

import scala.xml.Node
import scalax.io._

trait Request {
  def path: List[String]
  def uri: String
  def method : String
  def contextPath : String
  def queryString: Option[String]
  def param(name: String): List[String]
  def params: Map[String, List[String]]
  def header(name: String): Option[String]
  def headers: Map[String, String]
  def contentLength: Option[Long]
  def contentType: Option[String]
  def cookies: Map[String, Cookie]
  def cookie(name: String): Option[Cookie]
  def readBody: Input
  def resource(path: String): Input
}

class RequestShell(in: Request) extends Request {
  def path = in path
  def uri = in uri
  def method = in method
  def contextPath = in contextPath
  def queryString = in queryString
  def param(name: String) = in param name
  def params = in params
  def header(name: String) = in header name
  def headers = in headers
  def contentLength = in contentLength
  def contentType = in contentType
  def cookies = in cookies
  def cookie(name: String) = in cookie name
  def readBody = in readBody
  def resource(path: String) = in resource path
}

trait Response {
  def code: Int
  def reason: String
  def headers: Map[String, String]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def writeBody(channel: Output)
}

trait Context {
  def resourceChannel(res: String): Option[Output]
  def resourceAsXml(res: String): Option[Node]
  def contextPath: String
}


object Cookie {
  def apply(name: String, value: String) =
    new Cookie(name, value, None, None, None, None, false, false)

  def apply(name: String, value: String, maxAge: Int) =
    new Cookie(name, value, None, None, Some(maxAge), None, false, false)
}

case class Cookie(name: String,
                  value: String,
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Int],
                  version: Option[Int],
                  secure : Boolean,
                  httpOnly: Boolean)

sealed trait HttpMethod {
  def is(name: String): Boolean
}
case object GET extends HttpMethod {
  def is(name: String) = name == "GET"
}
case object POST extends HttpMethod{
  def is(name: String) = name == "POST"
}
case object PUT extends HttpMethod{
  def is(name: String) = name == "PUT"
}
case object DELETE extends HttpMethod{
  def is(name: String) = name == "DELETE"
}
case object HEAD extends HttpMethod{
  def is(name: String) = name == "HEAD"
}


