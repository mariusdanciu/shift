package net.shift
package util

import java.io._
import scala.io._
import scala.xml.{NodeSeq}
import scala.xml.parsing._

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

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]

}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: A): M[A]

  def bind[A, B](m: M[A], f: A => M[B]): M[B]
}

trait CTMonad[M[_]] extends Monad[M] {
  def mult[A](m: M[M[A]]): M[A] = bind(m, (x: M[A]) => x)
}
