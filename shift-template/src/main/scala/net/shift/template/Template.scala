package net.shift
package template

import scala.util.parsing.combinator.Parsers
import java.io.StringReader
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader
import scala.util.Try
import scala.util.Success
import scala.xml.NodeSeq
import scala.xml.XML
import scala.util.Failure
import net.shift.common.XmlUtils
import scala.annotation.tailrec
import net.shift.io.FileSystem
import net.shift.common.Path
import net.shift.common.StringUtils
import common.State
import common.State.init
import common.State.put
import common.State.putOpt
import common.State.state
import scala.xml.Elem
import net.shift.security.Permission
import net.shift.loc.Language
import net.shift.security.User

object StringTemplate {
  implicit def defaultTemplateQuery(implicit fs: FileSystem): TemplateQuery = name => for {
    input <- fs reader Path(s"web/templates/$name.html")
    content <- StringUtils.load(input)
  } yield content

  def defaultSnippets[T]: Map[String, State[SnipState[T], NodeSeq]] = Map(
    "permissions" -> state[SnipState[T], NodeSeq] {
      case st @ SnipState(PageState(s, language, user), params, e: Elem) =>
        val perms = params map { Permission }
        val res = user match {
          case Some(u) => (u.requireAll(perms: _*) {
            (st, new Elem(e.prefix, e.label, e.attributes, e.scope, e.child: _*))
          }).getOrElse((st, NodeSeq.Empty))
          case None => (st, NodeSeq.Empty)
        }
        Success(res)
    })

}

class StringTemplate {

  def run[T](html: String,
             snippets: DynamicContent[T],
             state: PageState[T])(implicit finder: TemplateQuery,
                                  fs: FileSystem): Try[(PageState[T], String)] = {

    val snipMap = (StringTemplate.defaultSnippets[T] ++ snippets.toMap)

    def exec(state: PageState[T], contents: List[Content]): Try[(PageState[T], String)] = {

      contents match {
        case Nil => Success((state, ""))
        case Static(s) :: tail =>
          exec(state, tail) map { r => (r._1, s + r._2) }
        case Dynamic(name, params, markup) :: tail =>
          snipMap.get(name) match {
            case Some(snip) =>
              val xml = XML.load(new java.io.ByteArrayInputStream(markup.getBytes("UTF-8")))

              (for {
                (st, nodes) <- snip(SnipState(state, params, xml))
                val s = XmlUtils.mkString(nodes)
                (rs, rc) <- exec(st.state, tail)
              } yield {
                (rs, s + rc)
              }).recoverWith {
                case t => exec(state, tail) map { r => (r._1, s"ERROR : Failed to load snippet $name: $t" + r._2) }
              }

            case _ =>
              val msg = s"ERROR: Snippet '$name' was not found.\n" + markup
              exec(state, tail) map { r => (r._1, msg + r._2) }
          }
        case TemplateRef(name) :: tail =>
          (for {
            raw <- finder(name)
            (st, c) <- run(raw, snippets, state)
            (rs, rc) <- exec(st, tail)
          } yield {
            (rs, c + rc)
          }).recoverWith {
            case t => exec(state, tail) map { r => (r._1, s"ERROR : Failed to load template $name: $t" + r._2) }
          }
        case LocRef(name) :: tail =>
          val str = net.shift.loc.Loc.loc0(state.lang)(name).text
          exec(state, tail) map { r => (r._1, str + r._2) }
      }
    }

    val doc = new TemplateParser().parse(html)

    doc flatMap { d => exec(state, d.contents.toList) }

  }

}

class TemplateParser extends Parsers {
  type Elem = Char

  private def value = rep1(acceptIf(b => (b >= 'a' && b <= 'z') ||
    (b >= 'A' && b <= 'Z') ||
    (b >= '0' && b <= '9') || b == '_' || b == '/')(err => "Not a value character " + err)) ^^ { r => r }

  private def identifier = rep1(acceptIf(b => (b >= 'a' && b <= 'z') ||
    (b >= 'A' && b <= 'Z') ||
    (b >= '0' && b <= '9') || b == '_' || b == '-' || b == '/' || b == '.')(err => "Not a value character " + err)) ^^ { r => r }

  private def noCRLFSpace = accept(' ') | accept('\t')

  private def ws = rep(noCRLFSpace)

  private def comment: Parser[Static] = anyUntilSeq("-->") ^^ { l => Static("<!--" + l.mkString + "-->") }

  private def template: Parser[TemplateRef] = {
    (ws ~> acceptSeq("template:") ~> value <~ anyUntilSeq("-->")) ^^ { l => TemplateRef(l.mkString) }
  }

  private def loc: Parser[LocRef] = {
    (ws ~> acceptSeq("loc:") ~> identifier <~ anyUntilSeq("-->")) ^^ { l => LocRef(l.mkString) }
  }

  private def static: Parser[Content] = anyUntilSeq("<!--") ^^ {
    case s => Static(s)
  }

  private def paramsParser: Parser[List[String]] = ws ~> '(' ~> ws ~> repsep(value, ws ~> ',' <~ ws) <~ ')' ^^ {
    case l => l map { _ mkString }
  }

  private def snipEnd: Parser[Unit] = ws ~> acceptSeq("end") ~> ws ~> acceptSeq("-->") ^^ { case _ => () }

  private def dynamic: Parser[Content] = ((((ws ~> acceptSeq("snip:") ~> value)) ~ opt(paramsParser) <~ anyUntilSeq("-->")) ~
    anyUntilSeq("<!--") <~ snipEnd) ^^ {
      case name ~ params ~ d => Dynamic(name.mkString, params getOrElse Nil, d)
    }

  private def content: Parser[Document] = rep(static ~ opt(template | loc | dynamic | comment)) ^^ {
    case s => Document(s flatMap {
      case s ~ Some(d) => List(s, d)
      case s ~ _       => List(s)
    })
  }

  private def anyUntilSeq(seq: String): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {

      @tailrec
      def walk(i: Input, acc: StringBuilder): (Input, StringBuilder) = {
        if (!i.atEnd && !acc.endsWith(seq)) {
          walk(i.rest, acc + i.first)
        } else {
          (i, acc)
        }
      }

      val (i, res) = walk(in, new StringBuilder)

      if (i.atEnd) {
        if (res.isEmpty)
          Failure("No content found", i)
        else
          Success(res.toString, i)
      } else
        Success(res.delete(res.size - seq.length(), res.size).toString, i)
    }
  }

  def parse(html: String): Try[Document] = {
    content(new CharSequenceReader(html)) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, _) => scala.util.Failure(new Exception(f))
      case Error(f, _)   => scala.util.Failure(new Exception(f))
    }
  }

}

sealed trait Content

case class Static(text: String) extends Content
case class Dynamic(name: String, params: List[String], content: String) extends Content
case class TemplateRef(name: String) extends Content
case class LocRef(name: String) extends Content

case class Document(contents: Seq[Content])

case class PageState[T](initialState: T, lang: Language, user: Option[User])

object PageState {
  def apply[T](initialState: T, lang: Language) = new PageState(initialState, lang, None)
}

case class SnipState[T](state: PageState[T], params: List[String], node: NodeSeq)