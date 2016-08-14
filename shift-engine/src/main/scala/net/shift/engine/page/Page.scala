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

object Html5 {

  import StringTemplate._

  def pageFromFile[T](state: PageState[T],
                      path: Path,
                      snippets: DynamicContent[T])(implicit fs: FileSystem): Attempt =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, markup) <- new StringTemplate().run(content, snippets, state)
    } yield {
      _(Html5Response(IO.stringProducer(markup)))
    }

  def runPageFromFile[T](state: PageState[T],
                         path: Path,
                         snippets: DynamicContent[T])(implicit fs: FileSystem): Try[(PageState[T], NodeSeq)] =
    for {
      input <- fs reader path
      content <- StringUtils.load(input)
      (state, n) <- new StringTemplate().run(content, snippets, state)
    } yield (state, XML.load(new StringReader(n)))

}

