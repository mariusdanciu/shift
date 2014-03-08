package net.shift

import scala.util.Failure
import scala.util.Success
import scala.util.Try

package object engine {
  
  implicit def ex2Fail[T](in : Exception): Failure[T] = Failure[T](in) 
}