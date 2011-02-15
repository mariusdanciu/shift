package net.shift
package util


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

trait Generator[A, M[_]] {

  type Cont[T] = A => M[T]

  def unit[B](b: B): M[B]
  def map[B](f: A => B): Cont[B] = f andThen unit
  def flatMap[B](f: A => M[B]): Cont[B] = f

  def filter(f: A => Boolean): Generator[A, M]
}

trait Functor[A] {

  type M[A] <: Functor[A]

  def unit[B](b: B): M[B]

  //def fmap[B](f: A => B): M[A] => M[B]
}

trait Monad[A] extends Functor[A] {

  type M[A] <: Monad[A]
  
  def bind[B](m: M[A])(f: A => M[B]): M[B]

}


class Cont[R, A](val in: (A => R) => R) {
  type M[R, A] = Cont[R, A]

  def apply(f: A => R): R = in(f)

  def unit[B](b: B): M[R, B] = new M[R, B](f => f(b))

  def bind[B](m: M[R, A])(f: A => M[R, B]): M[R, B] = 
    new M[R, B](k => m(x => f(x)(k)))
 
  def >>=[B] (f: A => M[R, B]): M[R, B] = bind(this)(f)
 
  def flatMap[B] = >>=[B] _

  def map[B](f: A => B): M[R, B] = bind(this)(f andThen unit)
  
}


trait StateFunctor[+A, S] {
  type F[A, S] <: StateFunctor[A, S]

  def fmap[A1 >: A, B](f: A1 => B): F[A1, S] => F[B, S]
}

object State {

  def state[A, S](f: S => (A, S)): State[A, S] = new State( f )

  def init[S] = new State[S, S](s => (s, s))

  def unit[A, S](a: A) = new State[A, S](s => (a, s))

  def modify[S](f: S => S) = state[Unit, S](s => ((), f(s)))

  def modifyPf[S](f: PartialFunction[S, S]) = 
    state[Unit, S](s => ((), if (f isDefinedAt s) f(s) else s))

  def compute[B, S](f: S => B) = new State[B, S](s => (f(s), s))

}

class State[+A, S](val in: S => (A, S)) extends StateFunctor[A, S] {
  type M[A, S] = State[A, S]
  type F[A, S] = M[A, S]

  def apply(s: S): (A, S) = in(s)

  def unit[B](b: B): M[B, S] = new M[B, S](s => (b, s))

  def fmap[A1 >: A, B](f: A1 => B): F[A1, S] => F[B, S] = _ >>= (unit[B] _ compose f)

  def mult[A1 >: A](a: M[M[A1, S], S]): M[A1, S] = a >>= (x => x)

  def bind[T, B](m: M[T, S])(f: T => M[B, S]) : M[B, S] = 
    new M[B, S](s => {
      m(s) match {
        case (a, s1) => f(a)(s1)
      }
    });

  def >>=[B] (f: A => M[B, S]): M[B, S] = bind(this)(f)
 
  def flatMap[B] = >>=[B] _

  def map[B](f: A => B): M[B, S] = bind(this)(unit[B] _ compose f)
  
}


import http._
import java.io.InputStream
import shiftdefs._

object Main3 {

  def unit[R, B](b: B): Cont[R, B] = new Cont[R, B](a => a(b))

  def state[B, S](b: B): State[B, S] = new State[B, S](s => (b, s))

  def modifyState[A, S](a: A)(f : S => S): State[A, S] = 
    new State[A, S](s => (a, f(s)))


