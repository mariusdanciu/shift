package net.shift.http.test

import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Routes extends App {

  import RoutesImplicits._

  def service(v: List[String], s: Int, s2: Int, d: Double, p: Int) = {
    println(v)
    println(s)
    println(s2)
    println(d)
    println(p)
    v
  }

  val res = ("a" / PathNParam(2) / "b" / IntParam / IntParam / DoubleParam / IntParam) {
    service
  }

  val out = res.matching("a" :: "123" :: "s" :: "b" :: "2" :: "5" :: "3.14" :: "10" :: Nil)

  println(res.scheme)
  println(out)

}

object RoutesImplicits {
  implicit def string2Static(s: String): Static = Static(s)
  implicit def string2Route(s: String): PathDef0 = PathDef0(List(Static(s)))
  implicit def static2Route(s: Static): PathDef0 = PathDef0(List(s))
}

sealed trait PathSpec {
  def scheme: String
}

case class Static(name: String) extends PathSpec {
  type Data = String

  def extract(path: List[String]): Try[(Data, List[String])] = path match {
    case h :: tail => Success((h, tail))
    case l         => Failure(new Exception(s"$l did not match"))
  }

  def scheme = name
}

trait PathParam[A] extends PathSpec {
  def extract(path: List[String]): Try[(A, List[String])]
}

case object IntParam extends PathParam[Int] {

  def extract(path: List[String]) = path match {
    case h :: tail => Try { (h.toInt, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }

  def scheme = "{:int}"
}

case object DoubleParam extends PathParam[Double] {

  def extract(path: List[String]) = path match {
    case h :: tail => Try { (h.toDouble, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }

  def scheme = "{:double}"
}

case object StringParam extends PathParam[String] {

  def extract(path: List[String]) = path match {
    case h :: tail => Try { (h.toString, tail) }
    case l         => Failure(new Exception(s"$l did not match"))
  }

  def scheme = "{:string}"
}

case object TailPathParam extends PathParam[List[String]] {
  def extract(path: List[String]) = Success((path, Nil))
  def scheme = "{:[string]}"
}

case class PathNParam(numParts: Int) extends PathParam[List[String]] {

  def extract(path: List[String]) = if (path.size >= numParts) {
    Success(path.splitAt(numParts))
  } else {
    Failure(new Exception(s"$path did not match"))
  }
  def scheme = s"{:[String]($numParts)}"
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
  def /[C](p3: PathParam[C]) = PathDef3[A, B, C](elems :+ p3, p1, p2, p3)
  def apply[R](f: (A, B) => R) = Route2(this, f)
}

case class PathDef3[A, B, C](elems: List[PathSpec], p1: PathParam[A], p2: PathParam[B], p3: PathParam[C]) {
  def /(static: Static) = PathDef3[A, B, C](elems :+ static, p1, p2, p3)
  def /[D](p4: PathParam[D]) = PathDef4[A, B, C, D](elems :+ p4, p1, p2, p3, p4)
  def apply[R](f: (A, B, C) => R) = Route3(this, f)
}

case class PathDef4[A, B, C, D](elems: List[PathSpec],
                                p1: PathParam[A],
                                p2: PathParam[B],
                                p3: PathParam[C],
                                p4: PathParam[D]) {
  def /(static: Static) = PathDef4[A, B, C, D](elems :+ static, p1, p2, p3, p4)
  def /[E](p5: PathParam[E]) = PathDef5[A, B, C, D, E](elems :+ p5, p1, p2, p3, p4, p5)
  def apply[R](f: (A, B, C, D) => R) = Route4(this, f)
}

case class PathDef5[A, B, C, D, E](elems: List[PathSpec],
                                   p1: PathParam[A],
                                   p2: PathParam[B],
                                   p3: PathParam[C],
                                   p4: PathParam[D],
                                   p5: PathParam[E]) {
  def /(static: Static) = PathDef5[A, B, C, D, E](elems :+ static, p1, p2, p3, p4, p5)
  def apply[R](f: (A, B, C, D, E) => R) = Route5(this, f)
}

sealed trait Route[R] {

  def matching(path: List[String]): Try[R]

  def scheme: String

  protected def walk0(path: List[String], specs: List[PathSpec]): Try[Boolean] = (path, specs) match {

    case (Nil, Nil) => Success(true)
    case (h1 :: tail1, Static(v) :: tail2) => if (h1 == v)
      walk0(tail1, tail2)
    else
      Failure(new Exception(s"part $h1 != $v"))
    case (h1, h2) => Failure(new Exception(s"part $h1 != $h2"))

  }

  protected def walk[A](path: List[String], specs: List[PathSpec], pp: PathParam[A]): Try[(A, List[String], List[PathSpec])] = (path, specs) match {
    case (h1 :: tail1, Static(v) :: tail2) =>
      if (h1 == v)
        walk(tail1, tail2, pp)
      else
        Failure(new Exception(s"part $h1 != $v"))
    case (h, (p: PathParam[_]) :: tail2) =>
      if (p == pp)
        pp.extract(h) map { case (a, pth) => (a, pth, tail2) }
      else
        Failure(new Exception(s"path $p != $pp"))

    case (h1, h2) => Failure(new Exception(s"part $h1 != $h2"))
  }
}

case class Route0[R](rd: PathDef0, f: () => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {
    walk0(path, rd.elems) map { b => f() }
  }

  def scheme = rd.elems.map { _.scheme }.mkString("/")
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

  def scheme = rd.elems.map { _.scheme }.mkString("/")
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

  def scheme = rd.elems.map { _.scheme }.mkString("/")
}

case class Route3[A, B, C, R](rd: PathDef3[A, B, C], f: (A, B, C) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {

    for {
      (a, rest1, specs1) <- walk(path, rd.elems, rd.p1)
      (b, rest2, specs2) <- walk(rest1, specs1, rd.p2)
      (c, rest3, specs3) <- walk(rest2, specs2, rd.p3)
      _ <- walk0(rest3, specs3)
    } yield {
      f(a, b, c)
    }
  }

  def scheme = rd.elems.map { _.scheme }.mkString("/")
}

case class Route4[A, B, C, D, R](rd: PathDef4[A, B, C, D], f: (A, B, C, D) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {

    for {
      (a, rest1, specs1) <- walk(path, rd.elems, rd.p1)
      (b, rest2, specs2) <- walk(rest1, specs1, rd.p2)
      (c, rest3, specs3) <- walk(rest2, specs2, rd.p3)
      (d, rest4, specs4) <- walk(rest3, specs3, rd.p4)
      _ <- walk0(rest4, specs4)
    } yield {
      f(a, b, c, d)
    }
  }

  def scheme = rd.elems.map { _.scheme }.mkString("/")
}

case class Route5[A, B, C, D, E, R](rd: PathDef5[A, B, C, D, E], f: (A, B, C, D, E) => R) extends Route[R] {
  def matching(path: List[String]): Try[R] = {

    for {
      (a, rest1, specs1) <- walk(path, rd.elems, rd.p1)
      (b, rest2, specs2) <- walk(rest1, specs1, rd.p2)
      (c, rest3, specs3) <- walk(rest2, specs2, rd.p3)
      (d, rest4, specs4) <- walk(rest3, specs3, rd.p4)
      (e, rest5, specs5) <- walk(rest4, specs4, rd.p5)
      _ <- walk0(rest5, specs5)
    } yield {
      f(a, b, c, d, e)
    }
  }

  def scheme = rd.elems.map { _.scheme }.mkString("/")
}
