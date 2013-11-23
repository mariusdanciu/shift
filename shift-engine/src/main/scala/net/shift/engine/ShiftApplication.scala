package net.shift
package engine

import http._
import common._

object ShiftApplication {

  implicit def rule(r: Rule) = State.gets[Request, Rule](req => r)
  
  def service(in: AsyncResponse => Unit): Rule =
    request => Some(in)

  def serviceWithRequest(in: Request => AsyncResponse => Unit): Rule =
    request => Some(in(request))
}

trait ShiftApplication {
  def servingRule : State[Request, Rule]
}
