package net.shift
package template

import scala.xml._
import common._
import common.State._
import common.XmlUtils._
import net.shift.loc.Loc
import net.shift.loc.Language

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

  import Loc._

  def apply[T](snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) =
    new Template[T](snippets)(List(selector, byLocAttr))

  private def locSnippet[T] = state[SnipState[T], NodeSeq] {
    import XmlUtils._

    s => s match {
      case SnipState(_, locale, e: Elem) =>
        println(locale)
        for { l <- attribute(e, "data-loc") } yield {
          (s, new Elem(e.prefix, e.label, e.attributes.remove("data-loc"), e.scope, Text(loc0(locale)(l).text)))
        }
    }
  }

  private def byLocAttr[T]: Selectors.type#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "data-loc")
      ) yield Template.locSnippet[T]
    case _ => None
  }
}

/**
 * Template engine
 */
class Template[T](snippets: DynamicContent[T])(implicit selectors: List[Selectors.type#Selector[SnipState[T]]]) {
  import Template._

  private val snippetsMap = snippets toMap

  private def pushNode[T](e: NodeSeq) = state[SnipState[T], NodeSeq] {
    s =>
      e match {
        case el: Elem =>
          val el1 = el.removeAttr("data-snip")
          Some((SnipState(s.state, s.locale, el1.e), el1.e))
        case n => Some((SnipState(s.state, s.locale, n), n))
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

          selectors.map(_(snippetsMap get)(e)).find(s => !s.isEmpty).flatten match {
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
case class SnipState[T](state: T, locale: Language, node: NodeSeq)
