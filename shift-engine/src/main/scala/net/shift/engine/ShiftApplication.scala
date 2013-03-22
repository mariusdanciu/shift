package net.shift
package engine

import http._
import common._

object ShiftApplication {

  implicit def rule(r: Rule) = State.gets[Request, Rule](req => r)
  
  def service(in: AsyncResponse => Unit): Request => Option[AsyncResponse => Unit] =
    request => Some(in)

  def serviceWithRequest(in: Request => AsyncResponse => Unit): Request => Option[AsyncResponse => Unit] =
    request => Some(in(request))
}

trait ShiftApplication {
  def servingRule : State[Request, Rule]
}
