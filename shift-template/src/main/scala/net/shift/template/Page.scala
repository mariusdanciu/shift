package net.shift
package template

import common._
import scala.xml.NodeSeq

object Snippet {
  import State._

  def snip[T](name: String)(f: PageState[T] => (T, NodeSeq)) = new Snippet(name, state[PageState[T], NodeSeq] {
    s =>
      f(s) match {
        case (t, n) => Some((PageState(t, n), n))
      }
  })
  
  def snipNoState[T](name: String)(f: PageState[T] => NodeSeq) = new Snippet(name, state[PageState[T], NodeSeq] {
    s =>
      f(s) match {
        case n => Some((PageState(s.req, n), n))
      }
  })

  
}

case class Snippet[T](name: String, f: State[T, NodeSeq])

trait DynamicContent[T] {
  def snippets: List[Snippet[PageState[T]]]

  def toMap: Map[String, State[PageState[T], NodeSeq]] = snippets.map(s => (s.name, s.f)).toMap
}


