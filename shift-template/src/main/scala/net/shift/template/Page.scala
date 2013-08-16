package net.shift
package template

import common._
import scala.xml.NodeSeq

object Snippet {
  import State._

  def snip[T](name: String)(f: SnipState[T] => (T, NodeSeq)) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s =>
      f(s) match {
        case (t, n) => Some((SnipState(t, n), n))
      }
  })
  
  def snipNoState[T](name: String)(f: SnipState[T] => NodeSeq) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s =>
      f(s) match {
        case n => Some((SnipState(s.state, n), n))
      }
  })

  
}

case class Snippet[T](name: String, f: State[SnipState[T], NodeSeq])

trait DynamicContent[T] {
  def snippets: List[Snippet[T]]

  def toMap: Map[String, State[SnipState[T], NodeSeq]] = snippets.map(s => (s.name, s.f)).toMap
}


