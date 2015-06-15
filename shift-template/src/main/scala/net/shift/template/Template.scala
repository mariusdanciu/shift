package net.shift
package template

import scala.util.Success
import scala.util.Try
import scala.xml.Atom
import scala.xml.Elem
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text
import scala.xml._

import Binds.bind
import Template.Head
import common.State.init
import common.State.put
import common.State.putOpt
import common.State.state
import net.shift.common.Attributes
import net.shift.common.Xml
import net.shift.common.Path
import net.shift.common.State
import net.shift.common.XmlImplicits._
import net.shift.common.XmlUtils.elemByName
import net.shift.common.XmlUtils.load
import net.shift.common.XmlUtils.node
import net.shift.io.FileSystem
import net.shift.loc.Language
import net.shift.loc.Loc
import net.shift.loc.Loc.loc0
import net.shift.security.Permission
import net.shift.security.User
import net.shift.security._

/**
 * Holds various strategies on matching page nodes with snippets
 *
 */
trait Selectors {

  type LookUp[T] = String => Option[State[T, NodeSeq]]
  type Selector[T] = (LookUp[T]) => NodeSeq => Option[State[T, NodeSeq]]

  /**
   * The value of "data-snip" attribute determines the snippet that will be applied
   */
  def bySnippetAttr[T]: Selector[SnipState[T]] = snippets => in => {

    def filterSnip = state[SnipState[T], NodeSeq] {
      s =>
        s match {
          case SnipState(st, e: Elem) =>
            val el = new Elem(e.prefix, e.label, e.attributes.remove("data-snip"), e.scope, e.child: _*)
            Success(SnipState(st, el), el)
        }
    }

    in match {
      case e: Elem =>
        for (
          value <- e attr "data-snip";
          snippet <- snippets(value)
        ) yield filterSnip.flatMap { _ => snippet }
      case _ => None
    }
  }

}
private[template] trait DefaultSnippets extends TemplateUtils {
  import Loc._

  def locSnippet[T](implicit fs: FileSystem) = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, _), e: Elem) =>
          Try((for { l <- e attr "data-loc" } yield {
            (s, new Elem(e.prefix, e.label, e.attributes.remove("data-loc"), e.scope, Text(loc0(language)(l).text)))
          }) get)
      }
  }

  def uniqueUrlSnippet[T] = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, _), e: Elem) =>
          import Binds._
          import net.shift.common.XmlUtils

          val ne = bind(e) {
            case Xml(n, a, _) => {
              val u = a.attrs.get("data-unique")

              val attrs = (a.attrs - "data-unique").map {
                case (k, v) if (!u.find(_ == k).isEmpty) => (k, v + "?q=" + System.currentTimeMillis())
                case (k, v)                              => (k, v)
              }
              node(n, Attributes(attrs))
            }
          }
          ne.map((s, _))
      }
  }

  def permissionSnippet[T] = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, user), e: Elem) =>
          Try((for { l <- e attr "data-permissions" } yield {
            val otherPerms = l.split("\\s*,\\s*").map(Permission(_))

            user match {
              case Some(u) => (u.requireAll(otherPerms: _*) {
                (s, new Elem(e.prefix, e.label, e.attributes.remove("data-permissions"), e.scope, e.child: _*))
              }).getOrElse((s, NodeSeq.Empty))
              case None => (s, NodeSeq.Empty)
            }

          }) get)
      }
  }

  def notThesePermissionSnippet[T] = state[SnipState[T], NodeSeq] {
    s =>
      s match {
        case SnipState(PageState(_, language, user), e: Elem) =>
          Try((for { l <- e attr "data-notthesepermissions" } yield {
            val otherPerms = l.split("\\s*,\\s*").map(Permission(_))

            user match {
              case Some(u) =>
                (u.notThesePermissions(otherPerms: _*) {
                  (s, new Elem(e.prefix, e.label, e.attributes.remove("data-notthesepermissions"), e.scope, e.child: _*))
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

object Template extends DefaultSnippets {

  def apply[T](snippets: DynamicContent[T])(implicit finder: TemplateFinder, selector: Selectors#Selector[SnipState[T]], fs: FileSystem) = {
    val t = new Template[T](snippets)(finder, fs, List(byLocAttr, byUniqueAttr, byPermissionsAttr, selector))
    new Template[T](snippets)(finder, fs, List(byPermissionsAttr, byNotThesePermissionsAttr, byLocAttr, byUniqueAttr, byTemplateAttr(t, finder), selector))
  }

  private def byPermissionsAttr[T]: Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- e attr "data-permissions"
      ) yield permissionSnippet[T]
    case _ => None
  }

  private def byNotThesePermissionsAttr[T]: Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- e attr "data-notthesepermissions"
      ) yield notThesePermissionSnippet[T]
    case _ => None
  }

  private def byLocAttr[T](implicit fs: FileSystem): Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- e attr "data-loc"
      ) yield locSnippet[T]
    case _ => None
  }

  private def byUniqueAttr[T]: Selectors#Selector[SnipState[T]] = snippets => in =>
    in match {
      case e: Elem =>
        for (
          value <- e attr "data-unique"
        ) yield uniqueUrlSnippet[T]
      case _ => None
    }

  private def byTemplateAttr[T](template: Template[T], finder: TemplateFinder): Selectors#Selector[SnipState[T]] = snippets => in => in match {
    case e: Elem =>
      for (
        value <- e attr "data-template"
      ) yield templateSnippet[T](template, finder)
    case _ => None
  }

  implicit def defaultTemplateFinder(implicit fs: FileSystem): TemplateFinder = name => for {
    input <- fs reader Path(s"web/templates/$name.html")
    content <- load(input)
  } yield content
}

/**
 * Template engine
 */
class Template[T](snippets: DynamicContent[T])(implicit finder: TemplateFinder, fs: FileSystem, selectors: List[Selectors#Selector[SnipState[T]]]) {
  import Template._
  import Binds._

  private val snippetsMap = snippets toMap

  private def locAttr[T](e: NodeSeq) = {
    def locAttributes(in: Elem, l: Language): Elem = {
      node(in.label, Attributes(in.attributes).map {
        case (k, v) =>
          if (v.startsWith("loc:"))
            (k -> Loc.loc0(l)(v.substring(4))(fs).text)
          else
            (k -> v)
      }, in.child:_*)
    }

    state[SnipState[T], NodeSeq] {
      s =>
        e match {
          case el: Elem =>
            val t = locAttributes(el, s.state.lang);
            Success((SnipState(s.state, t), t))
          case n => Success((SnipState(s.state, n), n))
        }
    }
  }

  private def pushNode[T](e: NodeSeq) = state[SnipState[T], NodeSeq] {
    s =>
      e match {
        case el: Elem =>
          Success((SnipState(s.state, el), el))
        case n => Success((SnipState(s.state, n), n))
      }
  }

  private[template] def run(in: NodeSeq, replacements: Replacements): State[SnipState[T], NodeSeq] = {
    import Binds._
    
    in match {
      case Group(childs) => run(childs, replacements)
      case t: Atom[_]    => put[SnipState[T], NodeSeq](t)
      case Head(header) =>
        run(header ++ replacements.head, replacements).map { n => <head>{ n }</head> }
      case e: Elem =>
        val res = selectors.map(_(snippetsMap get)(e)).find(s => !s.isEmpty).flatten match {
          case Some(snippet) =>
            for {
              _ <- pushNode[T](e)
              t <- snippet
              r <- run(t, replacements)
            } yield r
          case _ =>
            val op1 = (for {
              id <- putOpt[SnipState[T], String](e attr "id")
              n <- putOpt[SnipState[T], NodeSeq](replacements(id))
              r <- run(n, replacements - id)
            } yield r)

            val op2 = (for {
              elem <- run(e.child, replacements)
            } yield new Elem(e.prefix, e.label, e.attributes, e.scope, elem: _*))

            op1 | op2
        }
        for {
          e <- res 
          t <- locAttr(e)
        } yield t
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

object TemplateAttr {
  def unapply(e: Elem): Option[String] = for {
    wrp <- e attr "data-template"
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
