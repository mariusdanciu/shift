package net.shift
package common

import scala.util.Failure

object ShiftFailure {
  def apply[T] = new ShiftFailure[T]("Not found")
  def apply[T](msg: String) = new ShiftFailure[T](msg)
  def toTry[T] = new ShiftFailure[T]("Not found").toTry
}

class ShiftFailure[+T](msg: String) extends RuntimeException(msg) with util.control.NoStackTrace {
  def toTry = Failure(this)
}

  