package net.shift
package engine

import http._
import net.shift.common.Log
import scala.util.Success
import scala.util.Failure

object Engine extends Log {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse) {

    app.servingRule.map {
      case Success(f) => f(response)
      case Failure(t) => error("Fail processing the request", t)
    } (request)

  }

}
