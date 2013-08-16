package net.shift
package template

import scala.xml._
import common._
import State._
import XmlUtils._

object Selectors {

  type LookUp[T] = String => Option[State[T, NodeSeq]]
  type Selector[T] = (LookUp[T]) => NodeSeq => Option[State[T, NodeSeq]]

  def bySnippetAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "shift", "snippet");
        snippet <- snippets(value)
      ) yield snippet
    case _ => None
  }

  def byClassAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem => for {
      value <- attribute(e, "class")
      cls <- ("\\s+".r split value).headOption
      snippet <- snippets(cls)
    } yield snippet

    case _ => None
  }

  def byIdAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem => for (
      node <- attribute(e, "id");
      snippet <- snippets(node mkString)
    ) yield snippet
    case _ => None
  }
}

object Template {

  def apply[T](selector: Selectors.type#Selector[SnipState[T]])(snippets: DynamicContent[T]) =
    new Template[T](snippets, selector)

  def pushNode[T](e: NodeSeq) = state[SnipState[T], NodeSeq] {
    s => Some((SnipState(s.state, e), e))
  }

  def popNode[T, K](pstate: State[SnipState[T], K]): State[SnipState[T], NodeSeq] =
    for {
      _ <- pstate
      k <- state[SnipState[T], NodeSeq] {
        s => Some((s, s.node))
      }
    } yield k

}

/**
 * Template
 *
 */
class Template[T](snippets: DynamicContent[T], selector: Selectors.type#Selector[SnipState[T]]) {
  import Template._

  private val snippetsMap = snippets toMap

  def run(in: NodeSeq): State[SnipState[T], NodeSeq] = {
    def nodeProc(n: NodeSeq): State[SnipState[T], NodeSeq] = {
      n match {
        case Group(childs) => run(childs)

        case e: Elem =>
          selector(snippetsMap get)(e) match {
            case Some(snippet) =>
              for {
                _ <- pushNode[T](e)
                snip <- snippet
                r <- run(snip)
              } yield r

            case _ => for {
              elem <- run(e.child)
            } yield new Elem(e.prefix, e.label, e.attributes, e.scope, elem: _*)
          }

        case e => pushNode[T](e)
      }
    }

    (State.put[SnipState[T], NodeSeq](NodeSeq.Empty) /: in)((a, e) =>
      for {
        as <- a
        el <- nodeProc(e)
      } yield as ++ el)
  }
}

object SnipNode {
  import scala.xml._

  def unapply(n: NodeSeq): Option[(String, Map[String, String], NodeSeq)] = {
    n match {
      case e: Elem => Some((e.label, e.attributes.asAttrMap, e.child))
      case _ => None
    }
  }
}

case class SnipState[T](state: T, node: NodeSeq)
