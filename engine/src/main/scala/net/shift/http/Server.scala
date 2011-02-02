package net.shift
package http

import net.shift.util._
import Util._
import Application._


private[http] trait Generator[A, M[_]] {
  def unit[B](b: B): M[B]
  def map[B](f: A => B): (A => M[B]) = f andThen unit 
  def flatMap[B](f: A => M[B]): (A => M[B]) = f
  def filter(f: A => Boolean): Generator[A, M]
}

private[http] trait Reader {
  def read = new Generator[Request, Option] {
    def unit[B](b: B): Option[B] = {
      if (b != null) Some(b) else None
    }

    def filter(f: Request => Boolean): Generator[Request, Option] = this
  }
}


private[http] object Server extends Reader {

  def boot(ctx: Context) = Application.context = ctx

  def run = {
    for {req <- read
         template <- Application.resolveResource(req.path)
        } yield {
      XhtmlResponse(template theSeq(0))
    }
  }
  
}


