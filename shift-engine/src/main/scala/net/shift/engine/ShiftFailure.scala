package net.shift
package engine

object ShiftFailure {
  def apply[T] = new ShiftFailure[T]("Not found")
  def apply[T](msg: String) = new ShiftFailure[T](msg)
}

class ShiftFailure[+T](msg: String) extends RuntimeException(msg) with util.control.NoStackTrace

  