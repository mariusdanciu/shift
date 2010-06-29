package net.shift {
package util {

object Util {

  def toOption[T](v: T): Option[T] = if (v == null) None else Some(v)

  /**
   * Convert a java.util.Enumeration to a List[T]
   */
  def enumToList[T](enum: _root_.java.util.Enumeration[T]): List[T] = {
    var l: List[T] = Nil
    while (enum.hasMoreElements) {
      val next = enum.nextElement
      l = next :: l
    }
    l
  }

  def applyPf[A, B](a: A)(pf: PartialFunction[A, B]): Option[B] = {
    if (pf.isDefinedAt(a)) Some(pf(a)) else None
  }

}

class Scope[T] {
  val tl = new ThreadLocal[T]()

  def apply[A](value: T)(f: => A): A = {
    val old = tl.get
    try {
      tl.set(value)
      f
    } finally {
      tl.set(old)
    }
  }

  def get: T = tl.get

}



}
}
