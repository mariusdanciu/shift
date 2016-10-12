package net.shift
package engine

import scala.concurrent._
import scala.util.Failure
import scala.util.Success
import net.shift.common.Config
import net.shift.common.DefaultLog
import net.shift.common.Log
import net.shift.security.SecurityFailure
import net.shift.common.ShiftFailure
import net.shift.http.Request
import net.shift.http._
import net.shift.http.Responses._

object Engine extends DefaultLog {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse)(implicit conf: Config) {

    try {
      app.servingRule(request) match {
        case Success((_, Success(f))) =>

          f(r => {
            info("Sending response " + r)
            response(r)
          })
        case Success((_, Failure(t @ ShiftFailure(msg)))) =>
          error("Fail processing the request ", t)
          response(serverError.withTextBody(msg))
        case Success((_, Failure(t))) =>
          error("Fail processing the request ", t)
          response(serverError)
        case Failure(t @ SecurityFailure(msg, 401)) =>
          warn(s"Authentication failure $msg", t)
          response(basicAuthRequired(msg, conf.string("auth.realm", "shift")))
        case Failure(t @ SecurityFailure(msg, status)) =>
          warn(s"Authentication failure $msg", t)
          response(textResponse(msg).withCode(status))
        case Failure(t @ ShiftFailure(msg)) =>
          error("Fail processing the request ", t)
          response(serverError.withTextBody(msg))
        case Failure(t) =>
          error("Fail processing the request ", t)
          response(serverError)
        case r =>
          error(r.toString())
      }
    } catch {
      case e: Exception => e.printStackTrace
    }
  }
}
