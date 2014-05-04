package net.shift
package engine

import http._
import net.shift.common.Log
import scala.util.Success
import scala.util.Failure
import scala.concurrent._
import net.shift.common.DefaultLog

object Engine extends DefaultLog {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse)(implicit ec: scala.concurrent.ExecutionContext) {
    future {
      app.servingRule.map {
        case Success(f) => f(response)
        case Failure(t) => error("Fail processing the request", t)
      }(request)
    }
  }

}
