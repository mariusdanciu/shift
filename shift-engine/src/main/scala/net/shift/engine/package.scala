package net.shift

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import net.shift.server.http.ResponseFunc
import net.shift.engine.utils.ServiceUtils

package object engine {
  type Attempt = Try[ResponseFunc => Unit]

  implicit def ex2Fail[T](in: Exception): Failure[T] = Failure[T](in)

  implicit def wrapAttempt(func: ResponseFunc => Unit): ServiceUtils = new ServiceUtils(func)
}