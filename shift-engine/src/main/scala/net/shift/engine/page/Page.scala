package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._
import net.shift.loc.Language
import net.shift.engine.http.Html5Response
import net.shift.engine.http.Html5Response
import scala.util.Try
import scala.util.Success
import net.shift.common.DefaultLog

object Html5 extends XmlUtils with Selectors with PathUtils {

  def pageFromFile[T](initialState: T,
    lang: Language,
    path: Path,
    snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]]): Rule =
    for {
      input <- fromPath(path)
      content <- load(input)
      n <- new Html5(initialState, lang, snippets).resolve(content)
    } yield _(Html5Response(n._2))

  def pageFromContent[T](initialState: T,
    lang: Language,
    content: => NodeSeq,
    snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]]): Rule =
    for {
      n <- new Html5(initialState, lang, snippets).resolve(content)
    } yield _(Html5Response(n._2))

  def runPageFromFile[T](initialState: T,
    lang: Language,
    path: Path,
    snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]]): Try[(SnipState[T], NodeSeq)] =
    for {
      input <- fromPath(path)
      content <- load(input)
      n <- new Html5(initialState, lang, snippets).resolve(content)
    } yield n

  def runPageFromContent[T](initialState: T,
    lang: Language,
    content: => NodeSeq,
    snippets: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]]): Try[(SnipState[T], NodeSeq)] =
    new Html5(initialState, lang, snippets).resolve(content)
}

class Html5[T](initialState: T, language: Language, content: DynamicContent[T])(implicit selector: Selectors#Selector[SnipState[T]]) extends DefaultLog {
  def resolve(markup: NodeSeq): Try[(SnipState[T], NodeSeq)] = {
    for {
      c <- (Template[T](content) run markup)(SnipState(initialState, language, NodeSeq.Empty))
    } yield c
  }
}
