package net.shift
package common

import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait Functor[F[_]] {
  def unit[A](a: A): F[A]
  def fmap[A, B](f: A => B): F[A] => F[B]
}

trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def <*>[A, B](f: F[A => B]): F[A] => F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](f: A => M[B]): M[A] => M[B]
  def join[A](mma: M[M[A]]): M[A] = flatMap((ma: M[A]) => ma)(mma)
  def map[A, B](f: A => B): M[A] => M[B] = fmap(f)

}

trait Traversing[F[_]] {
  def traverse[A, B, M[_]](f: A => M[B])(fa: F[A])(implicit m: ApplicativeFunctor[F]): M[F[B]] = sequence(m.fmap(f)(fa))
  def sequence[A, M[_]](fma: F[M[A]])(implicit m: ApplicativeFunctor[F]): M[F[A]]
}

trait Combinators[M[_]] {
  def >=>[A, B, C](f: A => M[B])(g: B => M[C]): A => M[C]
}

trait Semigroup[A] {
  def append(a: A, b: A): A
}

trait State[S, +A] {
  import State._

  def apply(s: S): Try[(S, A)]

  def map[B](f: A => B): State[S, B] = state {
    apply(_) map { case (s, a) => (s, f(a)) }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = state {
    apply(_) flatMap { case (st, a) => f(a)(st) }
  }

  def filter(f: A => Boolean): State[S, A] = state {
    apply(_) filter { case (s, a) => f(a) }
  }

  def withFilter(f: A => Boolean): State[S, A] = state {
    apply(_) filter { case (s, a) => f(a) }
  }

  def |[B >: A](other: State[S, B]): State[S, B] = state {
    x =>
      apply(x) match {
        case Failure(_) => other apply x
        case s => s
      }
  }

  def >=>[B, C](f: A => State[S, B])(g: B => State[S, C]): State[S, C] = state {
    apply(_) flatMap {
      case (s, a) =>
        (for {
          b <- f(a)
          c <- g(b)
        } yield c)(s)
    }
  }

}

trait Identity[M[_]] {
  def unit[A](a: A): M[A]
}

trait Bind[M[_]] {
  def flatMap[A, B](f: A => M[B]): M[A] => M[B]
}

trait Flat[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

object Traversing {
  
  implicit def listTrySequence[A](in: List[Try[A]]): Try[List[A]] = {
    (Try(Nil: List[A]) /: in){(a, e) => e match {
      case Success(s) =>  a map {_ ::: List(s)}
      case _ => a
    }}
    
  }
}

object Monad {
  def monad[M[_]](implicit id: Identity[M], flat: Flat[M], b: Bind[M]): Monad[M] = new Monad[M] {

    def unit[A](a: A): M[A] = id unit a

    def fmap[A, B](f: A => B): M[A] => M[B] = flat fmap f

    def flatMap[A, B](f: A => M[B]): M[A] => M[B] = b flatMap f

  }

}

object State {
  import Monad._

  implicit def stateMonad[S] = monad[({ type l[a] = State[S, a] })#l]

  implicit def stateBind[S]: Bind[({ type l[a] = State[S, a] })#l] = new Bind[({ type l[a] = State[S, a] })#l] {
    def flatMap[A, B](f: A => State[S, B]): State[S, A] => State[S, B] = s => s flatMap f
  }

  implicit def stateIdentity[S]: Identity[({ type l[a] = State[S, a] })#l] = new Identity[({ type l[a] = State[S, a] })#l] {
    def unit[A](a: A): State[S, A] = state {
      s => Success((s, a))
    }
  }

  implicit def stateFlat[S]: Flat[({ type l[a] = State[S, a] })#l] = new Flat[({ type l[a] = State[S, a] })#l] {
    def fmap[A, B](f: A => B): State[S, A] => State[S, B] = _ map f
  }

  def state[S, A](f: S => Try[(S, A)]): State[S, A] = new State[S, A] {
    def apply(s: S) = f(s)
  }

  def init[S] = state[S, S] {
    s => Success((s, s))
  }

  def initf[S](f: S => S) = state[S, S] {
    s => Success((f(s), f(s)))
  }

  def put[S] = state[S, Unit] {
    s => Success((s, ()))
  }

  def put[S, A](a: A) = state[S, A] {
    s => Success((s, a))
  }

  def modify[S](f: S => S) = state[S, Unit] {
    s => Success((f(s), ()))
  }

  def gets[S, A](f: S => A) = for (s <- init[S]) yield f(s)

}

