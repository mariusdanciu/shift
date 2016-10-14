package net.shift.http.test

import scala.util.Try

object Routes extends App {

  import RoutesImplicits._

  val route = "a" / IntParam / "b" / StringParam

  def service(v: Int, s: String) = println(v)

  val res = route { service }

  println(res)

  val route1 = "a" / PathNParam(3) / "b" / TailPathParam

  def service1(v: List[String], s: List[String]) = println(v)

  val res1 = route1 { service1 }

  println(res1)

}

object RoutesImplicits {
  implicit def string2Static(s: String): Static = Static(s)
  implicit def string2Route(s: String): PathDef0 = PathDef0(List(Static(s)))
  implicit def static2Route(s: Static): PathDef0 = PathDef0(List(s))
}

sealed trait PathElem
case class Static(name: String) extends PathElem
case object * extends PathElem

trait PathParam extends PathElem {
  type Data
}

case object IntParam extends PathParam {
  type Data = Int
}
case object StringParam extends PathParam {
  type Data = String
}

case object TailPathParam extends PathParam {
  type Data = List[String]
}

case class PathNParam(numParts: Int) extends PathElem with PathParam {
  type Data = List[String]
}

case class PathDef0(elems: List[PathElem]) {
  def /(static: Static) = PathDef0(elems :+ static)
  def /(p: PathParam) = PathDef1[p.type#Data](elems :+ p)
  def apply[R](f: () => R) = Route0(this, f)
}

case class PathDef1[A](elems: List[PathElem]) {
  def /(static: Static) = PathDef1[A](elems :+ static)
  def /(p: PathParam) = PathDef2[A, p.type#Data](elems :+ p)
  def apply[R](f: A => R) = Route1(this, f)
}

case class PathDef2[A, B](elems: List[PathElem]) {
  def /(static: Static) = PathDef2[A, B](elems :+ static)
  def apply[R](f: (A, B) => R) = Route2(this, f)
}

sealed trait Route[R] {
  def matching(path: List[String]): Try[R]
}

case class Route0[R](rd: PathDef0, f: () => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    ???
  }
}

case class Route1[A, R](rd: PathDef1[A], f: A => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    ???
  }
}
case class Route2[A, B, R](rd: PathDef2[A, B], f: (A, B) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    ???
  }
}

