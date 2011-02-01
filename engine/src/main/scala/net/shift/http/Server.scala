package net.shift
package http

import net.shift.util._
import Util._
import Application._


private[http] object Server {
 
  private def read = new Generator[Request, Option] {
    def unit[B](b: B): Option[B] = {
      if (b != null) Some(b) else None
    }
  }

  def boot(ctx: Context) = Application.context = ctx

  def run = {
    for (req <- read) yield {
      TextResponse("Echo " + req.path.toString)
    }
  }
  
}


abstract class Generator[A, M[_]] {
  def unit[B](b: B): M[B]
  def map[B](f: A => B): (A => M[B]) = f andThen unit 
}
