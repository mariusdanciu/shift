package net.shift {
package http {

import java.io._


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
  def apply(text: String) = new TextResponse(text, 200, None, Nil, Nil)
}

case class TextResponse(val text: String,
  val code: Int,
  val reason: Option[String],
  val headers: List[(String, String)], 
  val cookies: List[Cookie]) extends Response {
  
  def contentType: Option[String] = Some("text/plain")
  def writeBody(out: OutputStream) = out.write(text.getBytes("UTF-8"))

}

case object OkResponse extends Response with HeaderDefaults with NoContent {
  def code = 200
}

case class NotFoundResponse(text: String) extends Response with HeaderDefaults  {
  def code = 404
  def contentType: Option[String] = Some("text/plain")
  def writeBody(out: OutputStream) = out.write(text.getBytes("UTF-8"))
}


object PermRedirectResponse {
  def apply(where: String) = new PermRedirectResponse(where, Nil)
}

case class PermRedirectResponse(where: String, val cookies: List[Cookie]) extends Response 
  with NoReason with NoContent{
  
  val code = 301
  val headers = List(("location", where))
}

}
}
