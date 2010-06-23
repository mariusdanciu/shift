package net.shift {
package http {

import java.io.{InputStream, OutputStream}

trait Request {

  def uri: String
  def path: List[String]
  def method: String
  def contextPath: String
  def queryString: Option[String]
  def param(name: String): Option[String]
  def params(name: String): List[String]
  def params: List[(String, String)]
  def header(name: String): Option[String]
  def headers(name: String): List[String]
  def headers: List[(String, String)]
  def contentLength: Option[Long]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def cookie(name: String): Option[Cookie]
  def inputStream: InputStream 
}


trait Response {
  def code: Int
  def reason: Option[String] 
  def headers: List[String]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def drain(out: OutputStream)
}


object Cookie {
  def apply(name: String, value: String) =
    new Cookie(name, Some(value), None, None, None, None, None)

  def apply(name: String, maxAge: Int) =
    new Cookie(name, None, None, None, Some(maxAge), None, None)
}

/**
 * Repersents an immutable representation of an HTTP Cookie
 */
case class Cookie(name: String,
                  value: Option[String],
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Int],
                  version: Option[Int],
                  secure_? : Option[Boolean])

object Request {
  def unapply(req: Request): Option[(List[String], String)] = Some((req.path, req.method))

  def apply(req: Request): Request = new ReqShell(req)
}

class ReqShell(req: Request) extends Request {
  def uri = req.uri
  def path = req.path
  def method = req.method
  def contextPath = req.contextPath
  def queryString = req.queryString
  def param(name: String) = req.param(name)
  def params(name: String) = req.params(name)
  def params = req.params
  def header(name: String) = req.header(name)
  def headers(name: String) = req.headers(name)
  def headers = req.headers
  def contentLength = req.contentLength
  def contentType = req.contentType
  def cookies = req.cookies
  def cookie(name: String) = req.cookie(name)
  def inputStream = req.inputStream
}

}
}
