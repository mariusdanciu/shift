package net.shift
package engine

import net.shift.common._
import net.shift.server.http._

import scala.util.Success

object ShiftApplication {

  implicit def rule(r: Attempt): State[Request, Attempt] = State.gets[Request, Attempt](req => r)

  def service(in: ResponseFunc => Unit): Attempt = Success(in)

  def serve(response: Response) = service(_ (response))
}

trait ShiftApplication {
  app =>
  def servingRule: State[Request, Attempt]

  def shiftService(implicit conf: Config) = new HttpService {

    override def apply(req: Request): (ResponseFunc) => Unit = resp => {
      Engine.run(app)(req, resp)
    }
  }
}
