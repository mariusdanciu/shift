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

  def extract(path: List[String]): Try[(Data, List[String])]
}

case class Static(name: String) extends PathSpec {
  type Data = String

  def extract(path: List[String]): Try[(Data, List[String])] = path match {
    case h :: tail => Success((h, tail))
    case l         => Failure(new Exception(s"$l did not match"))
  }
}

trait PathParam extends PathSpec {
  type Data
}

case object IntParam extends PathParam {
  type Data = Int

  def extract(path: List[String]): Try[(Data, List[String])] = path match {
    case h :: tail => Try { (h.toInt, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }
}
case object StringParam extends PathParam {
  type Data = String

  def extract(path: List[String]): Try[(Data, List[String])] = path match {
    case h :: tail => Try { (h.toString, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }
}

case object TailPathParam extends PathParam {
  type Data = List[String]

  def extract(path: List[String]): Try[(Data, List[String])] = Success((path, Nil))
}

case class PathNParam(numParts: Int) extends PathSpec with PathParam {
  type Data = List[String]

  def extract(path: List[String]): Try[(Data, List[String])] = if (path.size >= numParts) {
    Success(path.splitAt(numParts))
  } else {
    Failure(new Exception(s"$path did not match"))
  }
}

case class PathDef0(elems: List[PathSpec]) {
  def /(static: Static) = PathDef0(elems :+ static)
  def /(p: PathParam) = PathDef1[p.type#Data](elems :+ p)
  def apply[R](f: () => R) = Route0(this, f)
}

case class PathDef1[A](elems: List[PathSpec]) {
  def /(static: Static) = PathDef1[A](elems :+ static)
  def /(p: PathParam) = PathDef2[A, p.type#Data](elems :+ p)
  def apply[R](f: A => R) = Route1(this, f)
}

case class PathDef2[A, B](elems: List[PathSpec]) {
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

    ((None: Option[A], path) /: rd.elems) {
      case ((data, path), spec) =>
        spec.extract(path) match {
          case Success((a: A, rest)) => (Some(a), rest)
          case _                  => (data, path)
        }
    }

    ???
  }
}
case class Route2[A, B, R](rd: PathDef2[A, B], f: (A, B) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    ???
  }
}

