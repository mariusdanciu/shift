package net.shift
package engine
package http

import scala.xml._
import common._
import net.shift.io.IO._
import net.shift.io.Cont
import net.shift.io.Data
import XmlUtils._

object TextResponse {
  def apply(s: String) = new TextResponse(stringProducer(s))
}

case class TextResponse(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

case class HtmlStaticResponse(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

object Html5Response {
  def apply(in: NodeSeq) = new Html5Response(new BinProducer {
    def apply[O](it: BinConsumer[O]): BinConsumer[O] = it match {
      case Cont(f) => f(Data(("<!DOCTYPE html>\n" + mkString(in)).getBytes("UTF-8")))
      case r       => r
    }
  })
}

case class Html5Response(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

object CSSResponse {
  def apply(text: String) = new CSSResponse(stringProducer(text))
}

class CSSResponse(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/css; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

object JsResponse {
  def apply(text: String) = new JsResponse(stringProducer(text))
}

class JsResponse(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("text/javascript; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

class ImageResponse(content: BinProducer, mime: String) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some(mime)
  def cookies = Nil
  def body = content
}

object JsonResponse {
  def apply(text: String) = new JsonResponse(stringProducer(text))
}

case class JsonResponse(content: BinProducer) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Nil
  def contentType = Some("application/json; charset=\"UTF-8\"")
  def cookies = Nil
  def body = content
}

object Resp {
  def apply(status: Int) = new Resp(status, None, Nil, Nil, emptyProducer)
  def apply(status: Int, headers: Header*) = new Resp(status, None, List(headers: _*), Nil, emptyProducer)
  def apply(status: Int, mime: String, content: BinProducer) = new Resp(status, Some(mime), Nil, Nil, content)

  def ok = Resp(200)
  def created = Resp(201)
  def accepted = Resp(202)

  def redirect(location: String) = Resp(302, None, List(Header("Location", location)), Nil, emptyProducer)

  def badRequest = Resp(400)

  def basicAuthRequired(msg: String, realm: String) =
    new Resp(401, None, List(Header("WWW-Authenticate", s"""Basic realm="$realm"""")), Nil, stringProducer(msg)) {
      override def contentType = Some("text/plain; charset=\"UTF-8\"")
    }

  def basicAuthRequired(realm: String) =
    new Resp(401, None, List(Header("WWW-Authenticate", s"""Basic realm="$realm"""")), Nil, emptyProducer)

  def paymentRequired = Resp(402)
  def forbidden = Resp(403)
  def notFound = Resp(404)
  def confilct = Resp(409)

  def serverError = Resp(500)
  def notImplemented = Resp(501)
  def serviceUnavailable = Resp(503)

}

case class Resp(status: Int, mime: Option[String], heads: List[Header], cooks: List[Cookie], content: BinProducer) extends Response {
  def code = status
  def reason = "OK"
  def headers = heads
  def contentType = mime
  def cookies = cooks
  def body = content
}