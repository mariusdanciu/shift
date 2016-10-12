package net.shift
package engine
package page

import scala.xml.NodeSeq
import net.shift.template._
import http._
import common._
import net.shift.loc.Language
import net.shift.engine.http._
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
import net.shift.security.Credentials
import HttpPredicates._
import net.shift.http.Request
import net.shift.http.Responses
import net.shift.http.ContentType

object Html5 {

  def servePage[T](uri: String, filePath: Path, snipets: DynamicContent[Request])(implicit fs: FileSystem, tq: TemplateFinder) = for {
    r <- path(uri)
  } yield {
    Html5.pageFromFile(PageState(r, r.language, None), filePath, snipets)
  }

  def servePageForUser[T](uri: String, filePath: Path, snipets: DynamicContent[Request])(
    implicit fs: FileSystem, tq: TemplateFinder,
    login: Credentials => Try[User],
    conf: Config) = for {
    r <- path(uri)
    u <- user
  } yield {
    Html5.pageFromFile(PageState(r, r.language, u), filePath, snipets)
  }

  def page[T](path: Path,
              snippets: DynamicContent[T])(
                implicit fs: FileSystem, tq: TemplateFinder): State[PageState[T], String] = state[PageState[T], String] {
    s =>
      for {
        input <- fs reader path
        content <- StringUtils.load(input)
        (state, n) <- Template().run(content, snippets, s)
      } yield (state, n)
  }

  def pageFromFile[T](state: PageState[T],
                      path: Path,
                      snippets: DynamicContent[T])(implicit fs: FileSystem, tq: TemplateFinder): Attempt =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, markup) <- Template().run(content, snippets, state)
    } yield {
      _(Responses.ok.withTextBody(markup).withMime(ContentType.TextHtml))
    }

  def runPageFromFile[T](state: PageState[T],
                         path: Path,
                         snippets: DynamicContent[T])(implicit fs: FileSystem, tq: TemplateFinder): Try[(PageState[T], NodeSeq)] =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, n) <- Template().run(content, snippets, state)
    } yield (state, XML.load(new StringReader(n)))

}

