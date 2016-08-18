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

object Template {
  implicit def defaultTemplateQuery(implicit fs: FileSystem): TemplateFinder = name => for {
    input <- fs reader Path(s"web/templates/$name.html")
    content <- StringUtils.load(input)
  } yield content

  def defaultSnippets[T]: Map[String, State[SnipState[T], NodeSeq]] = Map(
    "permissions" -> state[SnipState[T], NodeSeq] {
      case st @ SnipState(PageState(s, language, user), params, e) =>
        val perms = params map { Permission }
        val res = user match {
          case Some(u) => (u.requireAll(perms: _*) {
            (st, e)
          }).getOrElse((st, NodeSeq.Empty))
          case None => (st, NodeSeq.Empty)
        }
        Success(res)
    })

  def apply() = new Template
}

class Template {

  def run[T](html: String,
             snippets: DynamicContent[T],
             state: PageState[T])(implicit finder: TemplateFinder,
                                  fs: FileSystem): Try[(PageState[T], String)] = {

    val snipMap = (Template.defaultSnippets[T] ++ snippets.snippetsMap)

    def exec(state: PageState[T], contents: Seq[Content]): (PageState[T], String, Seq[Content]) = {

      if (contents.isEmpty)
        (state, "", Nil)
      else {

        val (newState, result, rest) = contents match {
          case Static(s) :: tail =>
            val (st, content, rest) = exec(state, tail)
            (st, s + content, rest)
          case Comment(s) :: tail =>
            val (st, content, rest) = exec(state, tail)
            (st, "<!--" + s + "-->" + content, rest)
          case TemplateRef(name) :: tail =>
            lazy val (st, content, rest) = exec(state, tail)
            (for {
              raw <- finder(name)
              (ns, c) <- run(raw, snippets, state)
            } yield {
              (ns, c + content, rest)
            }).recover {
              case t => (st, s"ERROR : Failed to run template $name: $t", rest)
            } get
          case LocRef(name) :: tail =>
            val s = net.shift.loc.Loc.loc0(state.lang)(name).text
            val (st, content, rest) = exec(state, tail)
            (st, s + content, rest)
          case InlineRef(name, params) :: tail =>
            lazy val (st, content, rest) = exec(state, tail)
            snippets.inlinesMap.get(name) match {
              case Some(inl) =>
                (for {
                  (_, s) <- inl(InlineState(state, params))
                } yield {
                  (st, s + content, rest)
                }).recover {
                  case t => (st, s"ERROR : Failed to run inline $name: $t" + content, rest)
                } get

              case _ => (state, s"ERROR: Inline '$name' was not found.\n" + content, rest)
            }
          case SnipStart(name, params) :: tail =>
            val (st, content, next) = exec(state, tail)

            val (ns, res) = snipMap.get(name) match {
              case Some(snip) =>
                val xml = XML.load(new java.io.ByteArrayInputStream(("<group>" + content + "</group>").getBytes("UTF-8"))).child
                (for {
                  (st, nodes) <- snip(SnipState(state, params, xml))
                  val s = XmlUtils.mkString(nodes)
                } yield {
                  (st.state, s)
                }).recover {
                  case t => (state, s"ERROR : Failed to run snippet $name: $t")
                } get
              case _ => (state, s"ERROR : Snippet '$name' was not found.\n")
            }

            val (fs, c, rest) = exec(ns, next)
            (fs, res + c, rest)
          case SnipEnd() :: tail => (state, "", tail)

        }
        (newState, result, rest)
      }
    }

    val doc = new TemplateParser().parse(html)

    doc map { d =>
      val (st, acc, rest) = exec(state, d.contents.toList)
      (st, acc)
    }

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

  private def str(s: String) = ws ~> acceptSeq(s) <~ ws

  private def ws = rep(noCRLFSpace)

  private def comment: Parser[Comment] = str("<!--") ~> until(acceptSeq("-->"), false) ^^ { l => Comment(l.mkString) }

  private def template: Parser[TemplateRef] = {
    (str("<!--") ~> str("template") ~ str(":") ~> value <~ str("-->")) ^^ { l => TemplateRef(l.mkString) }
  }

  private def loc: Parser[LocRef] = {
    (str("<!--") ~> str("loc") ~> str(":") ~> identifier <~ str("-->")) ^^ { l => LocRef(l.mkString) }
  }

  def inline: Parser[InlineRef] = {
    (str("<!--") ~> str("inline") ~> str(":") ~> value ~ opt(params) <~ str("-->")) ^^ {
      case l ~ params => InlineRef(l.mkString, params getOrElse Nil)
    }
  }

  private def static: Parser[Content] = until(acceptSeq("<!--"), true) ^^ {
    case s =>
      Static(s)
  }

  private def params: Parser[List[String]] = str("(") ~> repsep(value, str(",")) <~ str(")") ^^ {
    case l => l map { _ mkString }
  }

  private def snipStart: Parser[SnipStart] = str("<!--") ~> str("snip") ~> str(":") ~> value ~ opt(params) <~ str("-->") ^^ {
    case l ~ params => SnipStart(l.mkString, params getOrElse Nil)
  }
  private def snipEnd: Parser[SnipEnd] = str("<!--") ~> str("end") ~> str("-->") ^^ { case _ => SnipEnd() }

  private def content: Parser[Document] = rep(template | loc | inline | snipStart | snipEnd | comment | static) ^^ {
    case s => Document(s)
  }

  private def until[T](p: Parser[T], retryPInput: Boolean): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {

      @tailrec
      def walk(i: Input, acc: StringBuilder): (Input, StringBuilder) = {
        val inBeforeP = i
        val r = p(i)

        if (!i.atEnd && !r.successful) {
          walk(i.rest, acc + i.first)
        } else {
          if (retryPInput)
            (inBeforeP, acc)
          else
            (r.next, acc)
        }
      }

      val (i, res) = walk(in, new StringBuilder)

      if (i.atEnd) {
        if (res.isEmpty)
          Failure("No content found", i)
        else
          Success(res.toString, i)
      } else
        Success(res.toString, i)

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
case class Comment(text: String) extends Content
case class SnipStart(name: String, params: List[String]) extends Content
case class SnipEnd() extends Content
case class TemplateRef(name: String) extends Content
case class LocRef(name: String) extends Content
case class InlineRef(name: String, params: List[String]) extends Content

case class Document(contents: Seq[Content])

case class PageState[T](initialState: T, lang: Language, user: Option[User])

object PageState {
  def apply[T](initialState: T, lang: Language) = new PageState(initialState, lang, None)
}

case class SnipState[T](state: PageState[T], params: List[String], node: NodeSeq)
case class InlineState[T](state: PageState[T], params: List[String])
