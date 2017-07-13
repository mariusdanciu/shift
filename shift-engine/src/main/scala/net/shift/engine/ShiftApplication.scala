package net.shift
package engine

import net.shift.common._
import net.shift.server.http._

import scala.util.Success

object ShiftApplication {

  implicit def rule(r: Attempt): State[Request, Attempt] = State.gets[Request, Attempt](req => r)

  def service(in: AsyncResponse => Unit): Attempt = Success(in)

  def serve(response: Response) = service(_ (response))
}

trait ShiftApplication {
  def servingRule: State[Request, Attempt]

  def shiftService(req: Request)(resp: AsyncResponse)(implicit conf: Config): Unit = {
    Engine.run(this)(req, resp)
  }
}
