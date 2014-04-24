package net.shift
package engine
package http

import scala.xml._
import common._
import scalax.io._
import JavaConverters._
import AsInputConverter._

object TextResponse {
  def apply(text: String) = new TextResponse(text.getBytes("UTF8").asInput)
}

case class TextResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

case class HtmlStaticResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

case class Html5Response(content: NodeSeq) extends Response with XmlUtils {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    channel.write(("<!DOCTYPE html>\n" + mkString(content)).getBytes("UTF-8"))
  }
}
object CSSResponse {
  def apply(text: String) = new CSSResponse(text.getBytes("UTF8").asInput)
}

class CSSResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/css; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

object JsResponse {
  def apply(text: String) = new JSResponse(text.getBytes("UTF8").asInput)
}

class JSResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/javascript; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

class ImageResponse(content: Input, mime: String) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some(mime)
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

object JsonResponse {
  def apply(text: String) = new JsonResponse(text.getBytes("UTF8").asInput)
}

case class JsonResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("application/json; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

