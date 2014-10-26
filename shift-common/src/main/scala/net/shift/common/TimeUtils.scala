package net.shift.common

import scala.concurrent.duration._

trait TimeUtils {

  def duration[T](f: => T)(g: Duration => Unit) = {
    val start = System.currentTimeMillis()
    try {
      f
    } finally {
      val end = System.currentTimeMillis()
      g(Duration(end - start, MILLISECONDS))
    }
  }

}