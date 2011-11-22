package net.shift
package engine
package common

trait Functor[F[_]] {
  def unit[A](a: A) : F[A]
  def fmap[A, B](f: A => B) : F[A] => F[B]
}

trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def <*>[A, B](f: F[A => B]): F[A] => F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def bind[A, B](f: A => M[B]): M[A] => M[B]
  def flatMap[A, B] = bind _
  def map[A, B](f: A => B): M[A] => M[B] = fmap(f)
  def join[A](mma : M[M[A]]): M[A] = bind((ma: M[A]) => ma)(mma)
}

trait Combinators[M[_]] {
  def >=>[A, B, C](f: A => M[B])(g: B => M[C]): A => M[C]
  def >|>[A, B](f: A => M[B])(g: A => M[B]): A => M[B]
}

trait CombinatorsView[A, B, M[_]] {
  def to[C](g: B => M[C]): A => M[C] 
  def or(f: A => M[B]): A => M[B]
}

/** 
 * Concrete implementations
 *   
 */
object OptionMonad extends Monad[Option] with Combinators[Option] {
  def unit[A](a: A) : Option[A] = Some(a)
  def fmap[A, B](f: A => B) : Option[A] => Option[B] = 
    in => in.map(f)
  def bind[A, B](f: A => Option[B]): Option[A] => Option[B] =
    in => in.flatMap(a => f(a))
  def >=>[A, B, C](f: A => Option[B])(g: B => Option[C]): A => Option[C] = 
    a => for (b <- f(a); c <- g(b)) yield c
  def >|>[A, B](f: A => Option[B])(g: A => Option[B]): A => Option[B] =  
    a => f(a) orElse g(a)
}

class OptionCombinatorsView[A, B](f: A => Option[B]) extends CombinatorsView[A, B, Option] {
  def to[C](g: B => Option[C]): A => Option[C] = 
    OptionMonad.>=>(f)(g)
  def or(g: A => Option[B]): A => Option[B] = 
    OptionMonad.>|>(f)(g)
}
