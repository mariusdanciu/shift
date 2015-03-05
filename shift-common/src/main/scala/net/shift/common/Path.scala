package net.shift
package common

import scala.util.Try
import scala.util.Success

object Path {

  private def string2Path(s: String): Path = {
    s.split(":").toList match {
      case parts :: Nil => new PathObj(None, pathToList(parts))
      case scheme :: parts :: Nil => pathToList(parts) match {
        case "" :: rest => new PathObj(Some(scheme), pathToList(parts))
        case _          => EmptyPath
      }
      case _ => EmptyPath
    }
  }

  private def pathToList(path: String) = {
    val l = (path split "/").toList
    if (path.endsWith("/"))
      l ::: List("")
    else
      l
  }

  def apply(p: String): Path = if (p == null || p.isEmpty) EmptyPath else string2Path(p)

  def apply(l: Traversable[String]): Path = if (l == null || l.isEmpty) EmptyPath else new PathObj(None, l.toList)

  def unapply(p: Path): Option[(Option[String], List[String])] = Some((p.scheme, p.parts))

}

trait Path {
  def scheme: Option[String]
  def parts: List[String]

  def startsWith(p: Path) = if (this.scheme != p.scheme) false else parts.startsWith(p.parts, 0)

  def headingSlash = parts match {
    case Nil        => false
    case "" :: rest => true
    case l          => false
  }

  def trailingSlash = parts match {
    case Nil => false
    case l   => l.last == ""
  }

  def subPath(start: Int): Path = {
    if (start > 0 && start < parts.size)
      new PathObj(None, parts.slice(start, parts.size))
    else EmptyPath
  }

  def subPath(start: Int, end: Int): Path = {
    if (start > 0 && start < parts.size && end > start && end <= parts.size)
      new PathObj(None, parts.slice(start, end))
    else EmptyPath
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

  def +(part: String): Path

  def ++(other: Path): Path

}

case class PathObj(schemePart: Option[String], partsList: List[String]) extends Path {

  def scheme: Option[String] = schemePart
  def parts: List[String] = partsList

  def +(part: String) = new PathObj(scheme, parts ::: List(part))

  def ++(other: Path) = new PathObj(scheme, parts ::: other.parts)

  override def equals(o: Any) = o match {
    case o: Path => parts == o.parts
    case _       => false
  }

  override def toString = scheme match {
    case None      => parts.mkString("/")
    case Some(sch) => sch + ":" + parts.mkString("/")
  }
}

case object EmptyPath extends Path {
  def scheme = None
  def parts = Nil
  def +(part: String) = EmptyPath
  def ++(other: Path) = EmptyPath
}


