package net.shift
package http

import net.shift.util._
import Util._
import Application._


private[http] trait Generator[A, M[_]] {
  def unit[B](b: B): M[B]
  def map[B](f: A => B): (A => M[B]) = f andThen unit 
}

private[http] trait Reader {
  def read = new Generator[Request, Option] {
    def unit[B](b: B): Option[B] = {
      if (b != null) Some(b) else None
    }
  }
}


private[http] object Server extends Reader {
 
  def boot(ctx: Context) = Application.context = ctx

  def run = {
    for (req <- read) yield {
      TextResponse("Echo " + req.path.toString)
    }
  }
  
}


