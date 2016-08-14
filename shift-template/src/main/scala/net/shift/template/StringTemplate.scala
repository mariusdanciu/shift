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

object StringTemplate {
  implicit def defaultTemplateQuery(implicit fs: FileSystem): TemplateQuery = name => for {
    input <- fs reader Path(s"web/templates/$name.html")
    content <- StringUtils.load(input)
  } yield content

}

class StringTemplate {

  def run[T](html: String,
             snippets: DynamicContent[T],
             state: PageState[T])(implicit finder: TemplateQuery,
                                  fs: FileSystem): Try[(PageState[T], String)] = {

    val snipMap = snippets.toMap

    def exec(state: PageState[T], contents: List[Content]): Try[(PageState[T], String)] = {

      contents match {
        case Nil => Success((state, ""))
        case Static(s) :: tail =>
          exec(state, tail) map { r => (r._1, s + r._2) }
        case Dynamic(name, markup) :: tail =>
          snipMap.get(name) match {
            case Some(snip) =>
              val xml = XML.load(new java.io.ByteArrayInputStream(markup.getBytes("UTF-8")))
              snip(SnipState(state, xml)) match {
                case Success((st, nodes)) =>
                  val s = XmlUtils.mkString(nodes)
                  exec(st.state, tail) map { r => (r._1, s + r._2) }
                case Failure(f) => Failure(f)
              }
            case _ =>
              val msg = s"ERROR: Snippet '$name' was not found.\n" + markup
              exec(state, tail) map { r => (r._1, msg + r._2) }
          }
        case TemplateRef(name) :: tail =>
          finder(name) match {
            case Success(s) =>
              exec(state, tail) map { r => (r._1, s + r._2) }
            case Failure(t) => Failure(t)
          }

        case Loc(name) :: tail =>
          val str = net.shift.loc.Loc.loc0(state.lang)(name).text
          exec(state, tail) map { r => (r._1, str + r._2) }
      }
    }

    val doc = new SnippetsParser().parse(html)

    doc flatMap { d => exec(state, d.contents.toList) }

  }

}

class SnippetsParser extends Parsers {
  type Elem = Char

  def value = rep1(acceptIf(b => (b >= 'a' && b <= 'z') ||
    (b >= 'A' && b <= 'Z') ||
    (b >= '0' && b <= '9') || b == '_' || b == '/')(err => "Not a value character " + err)) ^^ { r => r }

  def identifier = rep1(acceptIf(b => (b >= 'a' && b <= 'z') ||
    (b >= 'A' && b <= 'Z') ||
    (b >= '0' && b <= '9') || b == '_' || b == '-' || b == '/' || b == '.')(err => "Not a value character " + err)) ^^ { r => r }

  def noCRLFSpace = accept(' ') | accept('\t')

  def ws = rep(noCRLFSpace)

  def comment: Parser[Static] = anyUntilSeq("-->") ^^ { l => Static("<!--" + l.mkString + "-->") }

  def template: Parser[TemplateRef] = {
    (ws ~> acceptSeq("template:") ~> value <~ anyUntilSeq("-->")) ^^ { l => TemplateRef(l.mkString) }
  }

  def loc: Parser[Loc] = {
    (ws ~> acceptSeq("loc:") ~> identifier <~ anyUntilSeq("-->")) ^^ { l => Loc(l.mkString) }
  }

  def static: Parser[Content] = anyUntilSeq("<!--") ^^ {
    case s => Static(s)
  }

  def dynamic: Parser[Content] = ((((ws ~> acceptSeq("snip:") ~> value)) <~ anyUntilSeq("-->")) ~ anyUntilSeq("<!--end-->")) ^^ {
    case name ~ d => Dynamic(name.mkString, d)
  }

  def content: Parser[Document] = rep(static ~ opt(template | loc | dynamic | comment)) ^^ {
    case s => Document(s flatMap {
      case s ~ Some(d) => List(s, d)
      case s ~ _       => List(s)
    })
  }

  def anyUntilSeq(seq: String): Parser[String] = new Parser[String] {
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
case class Dynamic(name: String, content: String) extends Content
case class TemplateRef(name: String) extends Content
case class Loc(name: String) extends Content

case class Document(contents: Seq[Content])

