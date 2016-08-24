package net.shift
package http

import net.shift.io.IO._

object Responses {

  def text(msg: String) = {
    val bd = HTTPBody(msg)
    HTTPResponse(code = 200,
      headers = List(HTTPHeader("Content-Type", "text/plain; charset=\"UTF-8\""),
        HTTPHeader("Content-Length", bd.size.toString)),
      body = bd)
  }

}