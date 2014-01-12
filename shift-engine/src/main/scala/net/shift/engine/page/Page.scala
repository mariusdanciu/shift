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

object Html5 {

  def apply(req: Request, path: Path,
    snippets: DynamicContent[Request])(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    for {
      input <- req.resource(path)
      n <- XmlUtils.load(input)
      c <- apply(req, snippets, n)(selector)
    } yield c

  def apply(req: Request,
    snippets: DynamicContent[Request],
    content: NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    Success(resp => resp(Html5Response(new Html5(req, req.language, snippets).resolve(content))))

  def apply[T](req: Request, initial: Request => T,
    path: Path,
    snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    for {
      input <- req.resource(path)
      n <- XmlUtils.load(input)
      c <- apply(req, initial, snippets, n)(selector)
    } yield c

  def apply[T](req: Request, initial: Request => T,
    snippets: DynamicContent[T],
    content: NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    Success(resp => resp(Html5Response(new Html5(initial(req), req.language, snippets).resolve(content))))

}

class Html5[T](initialState: T, locale: Language, content: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) {
  def resolve(markup: NodeSeq): NodeSeq =
    (for {
      c <- (Template[T](content) run markup)(SnipState(initialState, locale, NodeSeq.Empty))
    } yield c._2) getOrElse NodeSeq.Empty
}
