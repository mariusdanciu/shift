package net.shift
package engine

import http._
import common._
import scala.util.Success

object ShiftApplication {

  implicit def rule(r: Attempt) = State.gets[Request, Attempt](req => r)

  def service(in: AsyncResponse => Unit): Attempt = Success(in)

}

trait ShiftApplication {
  def servingRule: State[Request, Attempt]
}
