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
import net.shift.http.HTTPRequest
import net.shift.http._
import net.shift.http.Responses._

object Engine extends DefaultLog {

  def run(app: ShiftApplication)(request: HTTPRequest, response: AsyncResponse)(implicit conf: Config) {

    try {
      app.servingRule(request) match {
        case Success((_, Success(f))) =>
          f(response)
        case Success((_, Failure(ShiftFailure(msg)))) =>
          error("Fail processing the request " + msg)
          response(serverError.withTextBody(msg))
        case Success((_, Failure(t))) =>
          error("Fail processing the request ", t)
          response(serverError)
        case Failure(SecurityFailure(msg, 401)) =>
          warn(s"Authentication failure $msg")
          response(basicAuthRequired(msg, conf.string("auth.realm", "shift")))
        case Failure(SecurityFailure(msg, status)) =>
          warn(s"Authentication failure $msg")
          response(textResponse(msg).withCode(status))
        case Failure(ShiftFailure(msg)) =>
          error("Fail processing the request " + msg)
          response(serverError.withTextBody(msg))
        case Failure(t) =>
          error("Fail processing the request " + t)
          response(serverError)
        case r => error(r.toString())
      }
    } catch {
      case e: Exception => e.printStackTrace
    }
  }
}
