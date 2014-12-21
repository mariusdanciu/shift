package net.shift.common

import scala.util.Failure

object Implicits {
  implicit def ex2Failure[E <: Exception](e: E): Failure[E] = Failure(e)
}