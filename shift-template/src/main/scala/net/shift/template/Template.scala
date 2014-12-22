package net.shift
package template

import scala.util.Success
import scala.util.Try
import scala.xml._
import scala.xml.Elem
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text

import common.State
import common.State._
import net.shift.common.Path
import net.shift.common.PathUtils
import net.shift.common.State
import net.shift.common.XmlUtils
import net.shift.loc.Language
import net.shift.loc.Loc
import net.shift.loc.Loc.loc0
import net.shift.security.Permission
import net.shift.security.User

/**
 * Holds various strategies on matching page nodes with snippets
 *
 */
trait Selectors extends XmlUtils {

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

private[template] trait DefaultSnippets extends XmlUtils with PathUtils with TemplateUtils {
  import Loc._

  def locSnippet[T] = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, _), e: Elem) =>
          Try((for { l <- attribute(e, "data-loc") } yield {
            (s, new Elem(e.prefix, e.label, e.attributes.remove("data-loc"), e.scope, Text(loc0(language)(l).text)))
          }) get)
      }
  }

  def permissionSnippet[T] = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, user), e: Elem) =>
          Try((for { l <- attribute(e, "data-permissions") } yield {
            val otherPerms = l.split("\\s*,\\s*").map(Permission(_))

            user match {
              case Some(u) => (u.requireAll(otherPerms: _*) {
                (s, new Elem(e.prefix, e.label, e.attributes.remove("data-permissions"), e.scope, e.child:_*))
              }).getOrElse((s, NodeSeq.Empty))
              case None => (s, NodeSeq.Empty)
            }

          }) get)
      }
  }

  def templateSnippet[T](implicit template: Template[T], finder: TemplateFinder) = for {
    SnipState(PageState(_, language, _), e @ TemplateAttr(t)) <- init[SnipState[T]]
    n <- put[SnipState[T], NodeSeq](e removeAttr "data-template")
    found <- find(t, finder)
    r <- template.run(found, toReplacements(e))
  } yield r

  private def find[T](name: String, finder: TemplateFinder) = state[SnipState[T], NodeSeq] {
    s => finder(name).map((s, _))
  }

  private def toReplacements(in: NodeSeq): Replacements = {

    val Children(_, heads) = elemByName(in, "head") getOrElse <head></head>;
    val Children(_, childs) = elemByName(in, "body") getOrElse <body></body>;

    Replacements(heads, ((Map.empty: Map[String, NodeSeq]) /: childs) {
      case (a, e @ IdAttr(id)) => a + ((id, e))
      case (a, _)              => a
    })

  }

}

object Template extends XmlUtils with DefaultSnippets with PathUtils {

  def apply[T](snippets: DynamicContent[T])(implicit finder: TemplateFinder, selector: Selectors#Selector[SnipState[T]]) = {
    val t = new Template[T](snippets)(finder, List(selector, byLocAttr))
    new Template[T](snippets)(finder, List(selector, byLocAttr, byTemplateAttr(t, finder), byPermissionsAttr))
  }

  private def byPermissionsAttr[T]: Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "data-permissions")
      ) yield permissionSnippet[T]
    case _ => None
  }

  private def byLocAttr[T]: Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "data-loc")
      ) yield locSnippet[T]
    case _ => None
  }

  private def byTemplateAttr[T](template: Template[T], finder: TemplateFinder): Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "data-template")
      ) yield templateSnippet[T](template, finder)
    case _ => None
  }

  implicit val defaultTemplateFinder: TemplateFinder = name => for {
    input <- fromPath(Path(s"web/templates/$name.html"))
    content <- load(input)
  } yield content
}

/**
 * Template engine
 */
class Template[T](snippets: DynamicContent[T])(implicit finder: TemplateFinder, selectors: List[Selectors#Selector[SnipState[T]]]) extends XmlUtils {
  import Template._

  private val snippetsMap = snippets toMap

  private def pushNode[T](e: NodeSeq) = state[SnipState[T], NodeSeq] {
    s =>
      e match {
        case el: Elem =>
          val el1 = el.removeAttr("data-snip")
          Success((SnipState(s.state, el1.e), el1.e))
        case n => Success((SnipState(s.state, n), n))
      }
  }

  private[template] def run(in: NodeSeq, replacements: Replacements): State[SnipState[T], NodeSeq] = {
    in match {
      case Group(childs) => run(childs, replacements)
      case t: Atom[_]    => put[SnipState[T], NodeSeq](t)
      case Head(header) =>
        put[SnipState[T], NodeSeq](<head>{ header ++ replacements.head }</head>)
      case e: Elem =>
        selectors.map(_(snippetsMap get)(e)).find(s => !s.isEmpty).flatten match {
          case Some(snippet) =>
            for {
              _ <- pushNode[T](e)
              snip <- snippet
              r <- run(snip, replacements)
            } yield r
          case _ =>
            val op1 = (for {
              id <- putOpt[SnipState[T], String](e getAttr "id")
              n <- putOpt[SnipState[T], NodeSeq](replacements(id))
              r <- run(n, replacements - id)
            } yield r)

            val op2 = (for {
              elem <- run(e.child, replacements)
            } yield new Elem(e.prefix, e.label, e.attributes, e.scope, elem: _*))

            op1 | op2
        }
      case n: NodeSeq =>
        (State.put[SnipState[T], NodeSeq](NodeSeq.Empty) /: n)((a, e) =>
          for {
            as <- a
            el <- run(e, replacements)
          } yield as ++ el)
      case e => pushNode[T](e)
    }
  }

  /**
   * Run the template processing
   */
  def run(in: NodeSeq): State[SnipState[T], NodeSeq] =
    run(in, Replacements(NodeSeq.Empty, Map.empty))

}

case class Replacements(head: NodeSeq, fragments: Map[String, NodeSeq]) {
  def -(id: String) = Replacements(head, fragments - id)
  def apply(id: String) = fragments get id
}

object TemplateAttr extends XmlUtils {
  def unapply(e: Elem): Option[String] = for {
    wrp <- attribute(e, "data-template")
  } yield wrp
}

object SnipNode {
  import scala.xml._

  def unapply(n: NodeSeq): Option[(String, Map[String, String], NodeSeq)] = {
    n match {
      case e: Elem => Some((e.label, e.attributes.asAttrMap, e.child))
      case _       => None
    }
  }
}

/**
 * @param initialState - the user state that is propagated during the page rendering
 * @param language - the language used for potential localization
 * @param user - the User context
 */
case class PageState[T](initialState: T, lang: Language, user: Option[User])

object PageState {
  def apply[T](initialState: T, lang: Language) = new PageState(initialState, lang, None)
}

/**
 * @param node - the page element that needs to be transformed by this snippet
 */
case class SnipState[T](state: PageState[T], node: NodeSeq)
