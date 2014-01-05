package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._
import net.shift.loc.Language
import net.shift.engine.http.Html5Response

object Html5 {

  def apply(req: Request, path: Path,
    snippets: DynamicContent[Request])(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    apply(req, path, snippets, (p: Path) => XmlUtils.load(req.resource(p)))(selector)

  def apply(req: Request, path: Path,
    snippets: DynamicContent[Request],
    pageLoader: Path => NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    Some(resp => resp(Html5Response(new Html5(req, req.language, snippets).resolve(pageLoader(path)))))

  def apply[T](req: Request, initial: Request => T,
    path: Path,
    snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    apply(req, initial, path, snippets, (p: Path) => XmlUtils.load(req.resource(p)))(selector)

  def apply[T](req: Request, initial: Request => T,
    path: Path,
    snippets: DynamicContent[T],
    pageLoader: Path => NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    Some(resp => resp(Html5Response(new Html5(initial(req), req.language, snippets).resolve(pageLoader(path)))))

}

class Html5[T](initialState: T, locale: Language, content: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) {
  def resolve(markup: NodeSeq): NodeSeq =
    (for {
      c <- (Template[T](content) run markup)(SnipState(initialState, locale, NodeSeq.Empty))
    } yield c._2) getOrElse NodeSeq.Empty
}
