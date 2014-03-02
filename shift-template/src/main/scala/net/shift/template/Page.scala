package net.shift
package template

import scala.xml.NodeSeq
import common.State
import common.State.state
import net.shift.common.State
import scala.util.Success
import scala.util.Try
import scala.util.Failure

object Snippet {
  import State._

  def snip[T](name: String)(f: SnipState[T] => Try[(T, NodeSeq)]) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s => f(s) map { case (t, n) => (SnipState(t, s.language, n), n) }
  })

  def snipNoState[T](name: String)(f: SnipState[T] => Try[NodeSeq]) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s => f(s) map { n => (SnipState(s.state, s.language, n), n) }
  })

}

case class Snippet[T](name: String, f: State[SnipState[T], NodeSeq])

trait DynamicContent[T] {
  def snippets: List[Snippet[T]]

  def toMap: Map[String, State[SnipState[T], NodeSeq]] = snippets.map(s => (s.name, s.f)).toMap
}


