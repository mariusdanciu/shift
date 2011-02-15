package net.shift
package http

import scala.xml._
import util._

import State._

trait Reader {
  def read = new Generator[Request, Option] {
    def unit[B](b: B): Option[B] = {
      if (b != null) Some(b) else None
    }

    def filter(f: Request => Boolean): Generator[Request, Option] = {
      this
    }
  }

  def readRequest = state[Request, ServerState](s => (s.req, s))

  def template(n: Option[NodeSeq]) =  state[NodeSeq, ServerState](s => n match {
    case Some(node) => (n, s)
    case _ => 
  })

}
