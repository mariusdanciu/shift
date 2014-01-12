package net.shift
package engine

import http._
import common._
import scala.util.Success

object ShiftApplication {

  implicit def rule(r: Rule) = State.gets[Request, Rule](req => r)

  def service(in: AsyncResponse => Unit): Rule = Success(in)

}

trait ShiftApplication {
  def servingRule: State[Request, Rule]
}
