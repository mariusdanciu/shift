package net.shift
package http

import scala.xml._
import util._

import State._

trait Reader {

  def readRequest() =  state[Request, PipeLine](s => Some((s.req, s)))

  def template(n: NodeSeq) = state[NodeSeq, PipeLine](s => Some((n, s)))

}
