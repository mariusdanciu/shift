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

object Html5 extends Selectors {
  def pageFromFile[T](state: PageState[T],
                      path: Path,
                      snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]], fs: FileSystem): Attempt =
    for {
      input <- fs reader path
      content <- load(input)
      n <- new Html5(state, snippets).resolve(content)
    } yield {
      _(Html5Response(n._2))
    }

  def pageFromContent[T](state: PageState[T],
                         content: => NodeSeq,
                         snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]], fs: FileSystem): Attempt =
    for {
      n <- new Html5(state, snippets).resolve(content)
    } yield _(Html5Response(n._2))

  def runPageFromFile[T](state: PageState[T],
                         path: Path,
                         snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]], fs: FileSystem): Try[(SnipState[T], NodeSeq)] =
    for {
      input <- fs reader path
      content <- load(input)
      n <- new Html5(state, snippets).resolve(content)
    } yield n

  def runPageFromContent[T](state: PageState[T],
                            content: => NodeSeq,
                            snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]], fs: FileSystem): Try[(SnipState[T], NodeSeq)] =
    new Html5(state, snippets).resolve(content)
}

class Html5[T](state: PageState[T], content: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]], fs: FileSystem) extends DefaultLog {
  def resolve(markup: NodeSeq): Try[(SnipState[T], NodeSeq)] = {
    import Template._
    lazy val t = Template[T](content) run markup
    for {
      c <- t(SnipState(state, NodeSeq.Empty))
    } yield c
  }
}
