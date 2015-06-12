package net.shift
package engine

import scala.concurrent._
import scala.util.Failure
import scala.util.Success
import http._
import net.shift.common.Config
import net.shift.common.DefaultLog
import net.shift.common.Log
import net.shift.security.SecurityFailure
import net.shift.common.ShiftFailure

object Engine extends DefaultLog {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse)(implicit ec: scala.concurrent.ExecutionContext) {

    Future {
      try {
        app.servingRule(request) match {
          case Success((_, Success(f))) => f(response)
          case Success((_, Failure(ShiftFailure(msg)))) =>
            error("Fail processing the request " + msg)
            response(Resp.serverError.asText.withBody(msg))
          case Success((_, Failure(t))) =>
            error("Fail processing the request ", t)
            response(Resp.serverError)
          case Failure(SecurityFailure(msg, 401)) =>
            warn(s"Authentication failure $msg")
            response(Resp.basicAuthRequired(msg, Config.string("auth.realm", "shift")))
          case Failure(SecurityFailure(msg, status)) =>
            warn(s"Authentication failure $msg")
            response(TextResponse(msg).code(status))
          case Failure(ShiftFailure(msg)) =>
            error("Fail processing the request " + msg)
            response(Resp.serverError.asText.withBody(msg))
          case Failure(t) =>
            error("Fail processing the request " + t)
            response(Resp.serverError)
          case r => error(r.toString())
        }
      } catch {
        case e: Exception => e.printStackTrace
      }
    }
  }
}
