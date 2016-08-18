package net.shift
package engine
package page

import scala.xml.NodeSeq
import net.shift.template._
import http._
import common._
import net.shift.loc.Language
import net.shift.engine.http.Html5Response
import net.shift.engine.http.Html5Response
import scala.util.Try
import scala.util.Success
import net.shift.common.DefaultLog
import net.shift.security.User
import XmlUtils._
import net.shift.io.FileSystem
import scala.util.Failure
import net.shift.io.IO
import scala.xml.XML
import java.io.StringReader
import net.shift.common.State._

object Html5 {

  def page[T](path: Path,
              snippets: DynamicContent[T])(
                implicit fs: FileSystem, tq: TemplateQuery): State[PageState[T], String] = state[PageState[T], String] {
    s =>
      for {
        input <- fs reader path
        content <- StringUtils.load(input)
        (state, n) <- Template().run(content, snippets, s)
      } yield (state, n)
  }

  def pageFromFile[T](state: PageState[T],
                      path: Path,
                      snippets: DynamicContent[T])(implicit fs: FileSystem, tq: TemplateQuery): Attempt =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, markup) <- Template().run(content, snippets, state)
    } yield {
      _(Html5Response(IO.stringProducer(markup)))
    }

  def runPageFromFile[T](state: PageState[T],
                         path: Path,
                         snippets: DynamicContent[T])(implicit fs: FileSystem, tq: TemplateQuery): Try[(PageState[T], NodeSeq)] =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, n) <- Template().run(content, snippets, state)
    } yield (state, XML.load(new StringReader(n)))

}

