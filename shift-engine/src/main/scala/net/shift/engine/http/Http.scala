
package net.shift
package engine
package http

import scala.xml.Node
import io.{ReadChannel, WriteChannel}

trait Request {
  def path: String
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
  def readBody: ReadChannel
}

trait Response {
  def code: Int
  def reason: String
  def headers: Map[String, String]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def writeBody(channel: WriteChannel)
}

trait Context {
  def resourceChannel(res: String): Option[ReadChannel]
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

trait HttpMethod
case object GET extends HttpMethod
case object POST extends HttpMethod
case object PUT extends HttpMethod
case object DELETE extends HttpMethod
case object HEAD extends HttpMethod
