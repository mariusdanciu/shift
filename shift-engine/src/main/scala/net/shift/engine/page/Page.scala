package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._

object Html5 {

  def apply(path: Path,
    snippets: DynamicContent[Request])(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    req => apply(path, snippets, (p: Path) => XmlUtils.load(req.resource(p)))(selector)(req)

  def apply(path: Path,
    snippets: DynamicContent[Request],
    pageLoader: Path => NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[Request]]): Rule =
    req => Some(resp => resp(Html5Response(new Html5(req, snippets).resolve(pageLoader(path)))))

  def apply[T](initial: Request => T,
    path: Path,
    snippets: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    req => apply(initial, path, snippets, (p: Path) => XmlUtils.load(req.resource(p)))(selector)(req)

  def apply[T](initial: Request => T,
    path: Path,
    snippets: DynamicContent[T],
    pageLoader: Path => NodeSeq)(implicit selector: Selectors.type#Selector[SnipState[T]]): Rule =
    req => Some(resp => resp(Html5Response(new Html5(initial(req), snippets).resolve(pageLoader(path)))))

}

class Html5[T](initialState: T, content: DynamicContent[T])(implicit selector: Selectors.type#Selector[SnipState[T]]) {
  def resolve(markup: NodeSeq): NodeSeq =
    (for {
      c <- (Template[T](content) run markup)(SnipState(initialState, NodeSeq.Empty))
    } yield c._2) getOrElse NodeSeq.Empty
}
