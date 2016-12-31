package net.shift
package engine

import net.shift.common._
import net.shift.security.SecurityFailure
import net.shift.server.http.Responses._
import net.shift.server.http.{Request, _}

import scala.util.{Failure, Success}

object Engine {

  private val log = LogBuilder.logger(getClass.getName)

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse)(implicit conf: Config) {

    try {
      app.servingRule(request) match {
        case Success((_, Success(f))) =>

          f(r => {
            log.info("Sending response " + r)
            response(r)
          })
        case Success((_, Failure(t @ ShiftFailure(msg)))) =>
          log.error("Fail processing the request ", t)
          response(serverError.withTextBody(msg))
        case Success((_, Failure(t))) =>
          log.error("Fail processing the request ", t)
          response(serverError)
        case Failure(t @ SecurityFailure(msg, 401)) =>
          log.warn(s"Authentication failure $msg", t)
          response(basicAuthRequired(msg, conf.string("auth.realm", "shift")))
        case Failure(t @ SecurityFailure(msg, status)) =>
          log.warn(s"Authentication failure $msg", t)
          response(textResponse(msg).withCode(status))
        case Failure(t @ ShiftFailure(msg)) =>
          log.error("Fail processing the request ", t)
          response(serverError.withTextBody(msg))
        case Failure(t) =>
          log.error("Fail processing the request ", t)
          response(serverError)
        case r =>
          log.error(r.toString)
      }
    } catch {
      case e: Exception => log.fatal("FATAL", e)
    }
  }
}
