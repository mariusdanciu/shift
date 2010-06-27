package net.shift {
package http {

import net.shift.util.Util._
import Application._


private[http] object Server {

  def run(req: Request): Option[Response] = {
    val request = applyPf(Request(req))(rewrite)
    request.map { r =>
      val resp: Response = Request.currentRequest(r) {
        TextResponse("In development")
      }

      resp
    }
  }

  
}

}
}
