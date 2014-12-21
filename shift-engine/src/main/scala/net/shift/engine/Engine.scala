package net.shift
package engine

import http._
import net.shift.common.Log
import scala.util.Success
import scala.util.Failure
import scala.concurrent._
import net.shift.common.DefaultLog
import net.shift.security.SecurityFailure
import net.shift.common.Config

object Engine extends DefaultLog {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse)(implicit ec: scala.concurrent.ExecutionContext) {

    Future {
      app.servingRule(request) match {
        case Success((_, Success(f))) => f(response)
        case Success((_, Failure(t))) =>
          error("Fail processing the request " + t)
          response(Resp.serverError)
        case Failure(SecurityFailure(msg)) =>
          warn(s"Authentication failure $msg")
          response(Resp.basicAuthRequired(Config.string("auth.realm", "shift")))
        case Failure(t) =>
          error("Fail processing the request " + t)
          response(Resp.serverError)
      }
    }
  }
}
