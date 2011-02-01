package net.shift
package http

import net.shift.util._
import Util._
import Application._


private[http] object Server {
  var continuation : Request => Option[Response] = _

  private def read = new Generator[Request, Response, Option]( f => {
     continuation = f
     None
    }
  ) {
    def unit(b: Response): Option[Response] = {
      if (b != null) Some(b) else None
    }
  }

  def boot(ctx: Context) = Application.context = ctx

  def run {
    for (req <- read) yield {
      TextResponse("Echo " + req.path.toString)
    }
  }
  
}


abstract class Generator[A, B, M[_]](wrap :  (A => M[B]) => M[B]) {
  def unit(b: B): M[B]
  def map(f: A => B): M[B] = wrap ( f andThen unit )
}
