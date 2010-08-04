package net.shift
package template

import net.shift.http._
import net.shift.util._

import scala.xml._


trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: A): M[A]

  def map[A, B](f: A => B): M[B]

  def flatMap[A, B](f: A => M[B]): M[B]

  def filter[A](f: A => Boolean): M[A]

}


