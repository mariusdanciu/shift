package net.shift
package common

import scala.util.Failure

object ShiftFailure {
  def apply = new ShiftFailure("Not found")
  def toTry = new ShiftFailure("Not found").toTry
}

case class ShiftFailure(msg: String) extends RuntimeException(msg) with util.control.NoStackTrace {
  def toTry = Failure(this)
}

  