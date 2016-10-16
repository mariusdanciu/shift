package net.shift.http.test

import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Routes extends App {

  import RoutesImplicits._

  val route = "a" / IntParam / "b" / StringParam

  def service(v: Int, s: String) = println(v)

  val res = route { service }

  println(res)

}

object RoutesImplicits {
  implicit def string2Static(s: String): Static = Static(s)
  implicit def string2Route(s: String): PathDef0 = PathDef0(List(Static(s)))
  implicit def static2Route(s: Static): PathDef0 = PathDef0(List(s))
}

sealed trait PathSpec {
  type Data

}

case class Static(name: String) extends PathSpec {
  type Data = String

  def extract(path: List[String]): Try[(Data, List[String])] = path match {
    case h :: tail => Success((h, tail))
    case l         => Failure(new Exception(s"$l did not match"))
  }
}

trait PathParam[A] extends PathSpec {
  def extract(path: List[String]): Try[(A, List[String])]
}

case object IntParam extends PathParam[Int] {

  def extract(path: List[String]) = path match {
    case h :: tail => Try { (h.toInt, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }
}
case object StringParam extends PathParam[String] {

  def extract(path: List[String]) = path match {
    case h :: tail => Try { (h.toString, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }
}

case object TailPathParam extends PathParam[List[String]] {

  def extract(path: List[String]) = Success((path, Nil))
}

case class PathNParam(numParts: Int) extends PathParam[List[String]] {

  def extract(path: List[String]) = if (path.size >= numParts) {
    Success(path.splitAt(numParts))
  } else {
    Failure(new Exception(s"$path did not match"))
  }
}

case class PathDef0(elems: List[PathSpec]) {
  def /(static: Static) = PathDef0(elems :+ static)
  def /[T](p: PathParam[T]) = PathDef1[T](elems :+ p, p)
  def apply[R](f: () => R) = Route0(this, f)
}

case class PathDef1[A](elems: List[PathSpec], p: PathParam[A]) {
  def /(static: Static) = PathDef1[A](elems :+ static, p)
  def /[A2](p2: PathParam[A2]) = PathDef2[A, A2](elems :+ p2, p, p2)
  def apply[R](f: A => R) = Route1(this, f)
}

case class PathDef2[A, B](elems: List[PathSpec], p1: PathParam[A], p2: PathParam[B]) {
  def /(static: Static) = PathDef2[A, B](elems :+ static, p1, p2)
  def apply[R](f: (A, B) => R) = Route2(this, f)
}

sealed trait Route[R] {
  def matching(path: List[String]): Try[R]

  def walk0(path: List[String], specs: List[PathSpec]): Try[Boolean] = (path, specs) match {

    case (Nil, Nil) => Success(true)
    case (h1 :: tail1, Static(v) :: tail2) => if (h1 == v)
      walk0(tail1, tail2)
    else
      Failure(new Exception(s"part $h1 != $v"))
    case (h1, h2) => Failure(new Exception(s"part $h1 != $h2"))

  }

  def walk[A](path: List[String], specs: List[PathSpec], pp: PathParam[A]): Try[(A, List[String], List[PathSpec])] = (path, specs) match {
    case (h1 :: tail1, Static(v) :: tail2) if (h1 == v) => walk(tail1, tail2, pp)

    case (h, (p: PathParam[_]) :: tail2) if (p == pp)   => pp.extract(h) map { case (a, pth) => (a, pth, tail2) }
  }
}

case class Route0[R](rd: PathDef0, f: () => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    walk0(path, rd.elems) map { b => f() }
  }
}

case class Route1[A, R](rd: PathDef1[A], f: A => R) extends Route[R] {

  def matching(path: List[String]): Try[R] = {
    for {
      (v, rest2, specs2) <- walk(path, rd.elems, rd.p)
      _ <- walk0(rest2, specs2)
    } yield {
      f(v)
    }
  }
}
case class Route2[A, B, R](rd: PathDef2[A, B], f: (A, B) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {

    for {
      (a, rest, specs1) <- walk(path, rd.elems, rd.p1)
      (b, rest2, specs2) <- walk(rest, specs1, rd.p2)
      _ <- walk0(rest2, specs2)
    } yield {
      f(a, b)
    }
  }
}

