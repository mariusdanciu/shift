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

  def apply[T](selector: Selectors.type#Selector[PageState[T]])(snippets: DynamicContent[T]) =
    new Template[T](snippets, selector)

  /**
   * Returns the String representation of the 'nodes'
   *
   */
  def mkString(nodes: NodeSeq): String = (nodes flatMap {
    case Group(childs) => mkString(childs)
    case Text(str) => escape(str)
    case e: Unparsed => e mkString
    case e: PCData => e mkString
    case e: Atom[_] => escape(e.data.toString)
    case e: Comment => e mkString
    case e: Elem => {
      val name = if (e.prefix eq null) e.label else e.prefix + ":" + e.label
      val attrs = if (e.attributes ne null) e.attributes.toString
      "<" + name + attrs + ">" + mkString(e.child) + "</" + name + ">"
    }
    case k => k.getClass toString
  }) mkString

  private def escape(str: String): String = ("" /: str)(_ + escape(_))

  private def escape(c: Char): String = c match {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '&' => "&amp;"
    case '"' => "&quot;"
    case '\n' => "\n"
    case '\r' => "\r"
    case '\t' => "\t"
    case c if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) => c toString
    case _ => ""
  }
}

/**
 * Template
 *
 */
class Template[T](snippets: DynamicContent[T], selector: Selectors.type#Selector[PageState[T]]) {

  private val snippetsMap = snippets toMap

  def lift[T](s: State[T, NodeSeq]): State[T, NodeSeq] = {
    for {
      nodeSeq <- s
    } yield { nodeSeq }
  }

  private def exposeState[T](st: State[T, NodeSeq]): State[T, T] = state {
    s => st(s) map { t => (t._1, t._1) }
  }

  def run(in: NodeSeq): State[PageState[T], NodeSeq] = {
    def nodeProc(in: State[PageState[T], NodeSeq], n: NodeSeq): State[PageState[T], NodeSeq] = {
      n match {
        case Group(childs) => run(childs)

        case e: Elem =>
          val st = state[PageState[T], NodeSeq] {
            s => Some((PageState(s.req, e), e))
          }

          selector(snippetsMap get)(e) match {
            case Some(snippet) =>
              for {
                _ <- st
                snip <- snippet
                e <- run(snip)
              } yield e

            case _ => for {
              elem <- run(e.child)
            } yield new Elem(e.prefix, e.label, e.attributes, e.scope, elem: _*)
          }

        case e => State put e
      }
    }

    (State.put[PageState[T], NodeSeq](NodeSeq.Empty) /: in)((a, e) =>
      for {
        as <- a
        el <- nodeProc(a, e)
      } yield as ++ el)
  }
  /**
   * Runs the in template and produces the modified template
   *
   *
   * def run(in: NodeSeq): NodeSeq = in flatMap {
   * case Group(childs) => run(childs)
   *
   * case e: Elem =>
   * (NodeSeq.Empty /: selectors)((a, s) =>
   * s(snippetsMap get)(e) match {
   * case Some(snippet) => a ++ run(snippet(e))
   * case _ => new Elem(e.prefix, e.label, e.attributes, e.scope, run(e.child): _*)
   * })
   *
   * case e => e
   * }
   */
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

case class PageState[T](req: T, node: NodeSeq)
