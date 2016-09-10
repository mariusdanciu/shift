package net.shift
package engine

import http._
import common._
import scala.util.Success
import net.shift.http._

object ShiftApplication {

  implicit def rule(r: Attempt) = State.gets[HTTPRequest, Attempt](req => r)

  def service(in: AsyncResponse => Unit): Attempt = Success(in)

}

trait ShiftApplication {
  def servingRule: State[HTTPRequest, Attempt]

  def shiftService(req: HTTPRequest)(resp: AsyncResponse)(implicit conf: Config): Unit = {
    Engine.run(this)(req, resp)
  }
}
