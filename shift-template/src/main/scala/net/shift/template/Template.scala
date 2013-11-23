package net.shift
package template

import scala.xml._
import common._
import State._
import XmlUtils._

/**
 * Holds various strategies on matching page nodes with snippets
 *
 */
object Selectors {

  type LookUp[T] = String => Option[State[T, NodeSeq]]
  type Selector[T] = (LookUp[T]) => NodeSeq => Option[State[T, NodeSeq]]

  /**
   * The value of "data-snip" attribute determines the snippet that will be applied
   */
  def bySnippetAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "data-snip");
        snippet <- snippets(value)
      ) yield snippet
    case _ => None
  }

  /**
   * The value of "class" attribute determines the snippet that will be applied
   */
  def byClassAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem => for {
      value <- attribute(e, "class")
      cls <- ("\\s+".r split value).headOption
      snippet <- snippets(cls)
    } yield snippet

    case _ => None
  }

  /**
   * The value of "id" attribute determines the snippet that will be applied
   */
  def byIdAttr[T]: Selector[T] = snippets => in => in match {
    case e: Elem => for (
      node <- attribute(e, "id");
      snippet <- snippets(node mkString)
    ) yield snippet
    case _ => None
  }
}

object Template {

  def apply[T](snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) =
    new Template[T](snippets)

}

/**
 * Template engine
 */
class Template[T](snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) {
  import Template._

  private val snippetsMap = snippets toMap

  private def pushNode[T](e: NodeSeq) = state[SnipState[T], NodeSeq] {
    s =>
      e match {
        case el: Elem =>
          val el1 = el.removeAttr("data-snip")
          Some((SnipState(s.state, el1.e), el1.e))
        case n => Some((SnipState(s.state, n), n))
      }
  }

  private def popNode[T, K](pstate: State[SnipState[T], K]): State[SnipState[T], NodeSeq] =
    for {
      _ <- pstate
      k <- state[SnipState[T], NodeSeq] {
        s => Some((s, s.node))
      }
    } yield k

  /**
   * Run the template processing
   */
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

/**
 * @param state - the user state that is propagated during the page rendering
 * @param node - the page element that needs to be transformed by this snippet
 */
case class SnipState[T](state: T, node: NodeSeq)
