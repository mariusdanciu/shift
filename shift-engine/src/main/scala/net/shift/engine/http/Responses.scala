package net.shift
package engine
package http

import scala.xml._
import common._
import scalax.io._
import JavaConverters._
import AsInputConverter._
import scalax.io.unmanaged.ReadableByteChannelResource

object TextResponse {
  def apply(text: String) = new TextResponse(text.getBytes("UTF8").asInput)
}

case class TextResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

case class HtmlStaticResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

case class Html5Response(content: NodeSeq) extends Response with XmlUtils {
  def code = 200
  def reason = "OK"
  def headers = Nil
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
  def headers = Nil
  def contentType = Some("text/css; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

object JsResponse {
  def apply(text: String) = new JsResponse(text.getBytes("UTF8").asInput)
}

class JsResponse(content: Input) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/javascript; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}

class ImageResponse(content: Input, mime: String) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
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
  def headers = Nil
  def contentType = Some("application/json; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: Output) = {
    content copyDataTo channel
  }
}
object Resp {
  def apply(status: Int) = new Resp(status, None, Nil, Nil, None)
  def apply(status: Int, headers: Header*) = new Resp(status, None, List(headers:_*), Nil, None)
  def apply(status: Int, mime: String, content: Input) = new Resp(status, Some(mime), Nil, Nil, Some(content))

  def ok = Resp(200)
  def created = Resp(201)
  def accepted = Resp(202)

  def redirect(location: String) = Resp(302, None, List(Header("Location", location)), Nil, None)

  def badRequest = Resp(400)
  def basicAuthRequired(msg: String, realm: String) =
    new Resp(401, None, List(Header("WWW-Authenticate", s"""Basic realm="$realm"""")), Nil, Some(msg.getBytes("UTF8").asInput)) {
      override def contentType = Some("text/plain; charset=\"UTF-8\"")
    }
  def basicAuthRequired(realm: String) =
    new Resp(401, None, List(Header("WWW-Authenticate", s"""Basic realm="$realm"""")), Nil, None)
  def paymentRequired = Resp(402)
  def forbidden = Resp(403)
  def notFound = Resp(404)
  def confilct = Resp(409)

  def serverError = Resp(500)
  def notImplemented = Resp(501)
  def serviceUnavailable = Resp(503)

}

case class Resp(status: Int, mime: Option[String], heads: List[Header], cooks: List[Cookie], content: Option[Input]) extends Response {
  def code = status
  def reason = "OK"
  def headers = heads
  def contentType = mime
  def cookies = cooks
  def writeBody(channel: Output) = for (c <- content) {
    c copyDataTo channel
  }
}