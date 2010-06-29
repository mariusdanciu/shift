package net.shift {
package http {

import java.io._
import scala.xml._

trait HeaderDefaults {
  val headers: List[(String, String)] = Nil
  val cookies: List[Cookie] = Nil
  val reason: Option[String] = None
}

trait NoContent {
  val contentType: Option[String] = None
  def writeBody(out: OutputStream) = {}
}

trait NoCookies {
  val cookies: List[Cookie] = Nil
}

trait NoReason {
  val reason: Option[String] = None
}

object TextResponse {
  def apply(text: String) = new TextResponse(text, 200, Nil, Nil)
}

case class TextResponse(val text: String,
  val code: Int,
  val headers: List[(String, String)], 
  val cookies: List[Cookie]) extends Response with NoReason {
  
  def contentType: Option[String] = Some("text/plain; charset=utf-8")
  def writeBody(out: OutputStream) = out.write(text.getBytes("UTF-8"))

}

case object OkResponse extends Response with HeaderDefaults with NoContent {
  def code = 200
}

case class NotFoundResponse(text: String) extends Response with HeaderDefaults  {
  def code = 404
  def contentType: Option[String] = Some("text/plain; charset=utf-8")
  def writeBody(out: OutputStream) = out.write(text.getBytes("UTF-8"))
}


object PermRedirectResponse {
  def apply(where: String) = new PermRedirectResponse(where, Nil)
}

case class PermRedirectResponse(where: String, val cookies: List[Cookie]) extends Response 
  with NoReason with NoContent {
  
  val code = 301
  val headers = List(("location", where))
}

object RedirectResponse {
  def apply(where: String) = new PermRedirectResponse(where, Nil)
}

case class RedirectResponse(where: String, val cookies: List[Cookie]) extends Response 
  with NoReason with NoContent{
  
  val code = 301
  val headers = List(("location", where))
}

abstract class NodeResponse(val node: Node,
  val code: Int,
  val headers: List[(String, String)], 
  val cookies: List[Cookie]) extends Response with NoReason {

  def encoding: Option[String] = Some("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

  def docType: Option[String]
  
  def contentType: Option[String] = Some("text/plain; charset=utf-8")

  def docTypeBeforeEncoding = false

  def writeBody(out: OutputStream) {
  
  }

}

}
}
