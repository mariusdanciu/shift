package net.shift
package common

import scala.util.Failure

object ShiftFailure {
  def apply = new ShiftFailure("Not found")
  def apply(msg: String) = new ShiftFailure(msg)
  def toTry = new ShiftFailure("Not found").toTry
}

class ShiftFailure(msg: String) extends RuntimeException(msg) with util.control.NoStackTrace {
  def toTry = Failure(this)
}

  