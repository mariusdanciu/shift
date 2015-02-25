package net.shift
package common

import PathUtils._

object Path {
  def apply(p: String) = new Path(pathToList(p))

  def apply(l: Traversable[String]): Path = new Path(l.toList)

  def unapply(l: Path): Option[List[String]] = Some(l.parts)
}

class Path(val parts: List[String]) {

  def startsWith(p: Path) = parts.startsWith(p.parts, 0)

  def trailingSlash = parts match {
    case Nil => false
    case l   => l.last == ""
  }

  def head: Option[String] = parts match {
    case h :: Nil => Some(h)
    case _        => None
  }

  def tail: Path = parts match {
    case Nil       => EmptyPath
    case h :: Nil  => EmptyPath
    case h :: rest => Path(rest)
  }

  def +(part: String) = new Path(parts ::: List(part))

  def ++(other: Path) = new Path(parts ::: other.parts)

  override def equals(o: Any) = o match {
    case o: Path => parts == o.parts
    case _       => false
  }

  override def toString = parts.mkString("/")
}

case object EmptyPath extends Path(Nil)


