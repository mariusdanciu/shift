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

  def inline[T](name: String)(f: InlineState[T] => Try[(T, String)]) = new Inline(name, state[InlineState[T], String] {
    s => f(s) map { case (t, n) => (InlineState(PageState(t, s.state.lang, s.state.user), Nil), n) }
  })

  def snip[T](name: String)(f: SnipState[T] => Try[(T, NodeSeq)]) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s => f(s) map { case (t, n) => (SnipState(PageState(t, s.state.lang, s.state.user), Nil, n), n) }
  })

  def snipNoState[T](name: String)(f: SnipState[T] => Try[NodeSeq]) = new Snippet(name, state[SnipState[T], NodeSeq] {
    s => f(s) map { n => (SnipState(s.state, Nil, n), n) }
  })

}

case class Snippet[T](name: String, f: State[SnipState[T], NodeSeq])
case class Inline[T](name: String, f: State[InlineState[T], String])

trait DynamicContent[T] {
  def snippets: List[Snippet[T]]
  def inlines: List[Inline[T]] = Nil

  def snippetsMap: Map[String, State[SnipState[T], NodeSeq]] = snippets.map((s: Snippet[T]) => (s.name, s.f)).toMap
  def inlinesMap: Map[String, State[InlineState[T], String]] = inlines.map((s: Inline[T]) => (s.name, s.f)).toMap
}


