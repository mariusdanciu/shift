package net.shift
package http

import net.shift.io.IO._

object Responses {

  def text(msg: String) = {
    val bd = HTTPBody(msg)
    HTTPResponse(code = 200,
      headers = List(TextHeader("Content-Type", "text/plain; charset=\"UTF-8\""),
        TextHeader("Content-Length", bd.size.toString)),
      body = bd)
  }

  def html(msg: String) = {
    val bd = HTTPBody(msg)
    HTTPResponse(code = 200,
      headers = List(TextHeader("Content-Type", "text/html; charset=\"UTF-8\""),
        TextHeader("Content-Length", bd.size.toString)),
      body = bd)
  }

  def css(msg: String) = {
    val bd = HTTPBody(msg)
    HTTPResponse(code = 200,
      headers = List(TextHeader("Content-Type", "text/css; charset=\"UTF-8\""),
        TextHeader("Content-Length", bd.size.toString)),
      body = bd)
  }

  def javaScript(msg: String) = {
    val bd = HTTPBody(msg)
    HTTPResponse(code = 200,
      headers = List(TextHeader("Content-Type", "text/javascript; charset=\"UTF-8\""),
        TextHeader("Content-Length", bd.size.toString)),
      body = bd)
  }

  def ok = HTTPResponse(code = 200, body = HTTPBody(Nil))

  def created = HTTPResponse(code = 201, body = HTTPBody(Nil))
  def accepted = HTTPResponse(code = 201, body = HTTPBody(Nil))

  def redirect(location: String) =
    HTTPResponse(code = 302, headers = List(TextHeader("Location", location)), body = HTTPBody(Nil))
  def notModified = HTTPResponse(code = 304, body = HTTPBody(Nil))

  def badRequest = HTTPResponse(code = 400, body = HTTPBody(Nil))

  def basicAuthRequired(msg: String, realm: String) =
    HTTPResponse(code = 401, headers = List(TextHeader("WWW-Authenticate", s"""Basic realm="$realm"""")), body = HTTPBody(msg))

  def paymentRequired = HTTPResponse(code = 402, body = HTTPBody(Nil))
  def forbidden = HTTPResponse(code = 403, body = HTTPBody(Nil))
  def notFound = HTTPResponse(code = 404, body = HTTPBody(Nil))
  def confilct = HTTPResponse(code = 409, body = HTTPBody(Nil))

  def serverError = HTTPResponse(code = 500, body = HTTPBody(Nil))
  def notImplemented = HTTPResponse(code = 501, body = HTTPBody(Nil))
  def serviceUnavailable = HTTPResponse(code = 503, body = HTTPBody(Nil))

}