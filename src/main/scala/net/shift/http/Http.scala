package net.shift {
package http {

import java.io.{InputStream, OutputStream}

trait Request {

  def uri: String
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


}
}
