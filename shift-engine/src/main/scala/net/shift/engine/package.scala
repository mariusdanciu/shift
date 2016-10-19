package net.shift

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import net.shift.server.http.AsyncResponse
import net.shift.engine.utils.ServiceUtils

package object engine {
  type Attempt = Try[AsyncResponse => Unit]

  implicit def ex2Fail[T](in: Exception): Failure[T] = Failure[T](in)

  implicit def wrapAttempt(func: AsyncResponse => Unit): ServiceUtils = new ServiceUtils(func)
}