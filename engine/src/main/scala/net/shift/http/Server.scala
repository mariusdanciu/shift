package net.shift {
package http {

import net.shift.util._
import Util._
import Application._


private[http] object Server {

  def boot(ctx: Context) = Application.context = ctx

  def run(req: Request): Option[Response] = {
    val request = applyPf(Request(req))(rewrite)
    for (r <- request) yield {
      Request.req(r) {
        TextResponse("In development")
      }
    }
  }

  
}

}
}
