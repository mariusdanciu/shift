package net.shift

import scala.util.Failure
import scala.util.Success
import scala.util.Try

package object engine {
  
  implicit def option2Try[T](o: Option[T]): Try[T] = o match {
    case Some(a) => Success(a)
    case None => ShiftFailure[T]
  }
 
  implicit def ex2Fail[T](in : Exception): Failure[T] = Failure[T](in) 
  
  
}