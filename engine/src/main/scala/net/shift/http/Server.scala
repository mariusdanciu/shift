package net.shift {
package http {

import net.shift.util._
import Util._
import Application._


private[http] object Server {

  def boot(ctx: Context) = Application.context = ctx

  def run(req: Request): Option[Response] = {

    for (r <- applyPf(Request(req))(rewrite)) yield {
        TextResponse("In development")
    }
  }

  
}

}
}
