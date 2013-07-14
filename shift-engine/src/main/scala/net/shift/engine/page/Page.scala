package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._

object Html5 {


  def apply(path: String, selectors: Selectors.type#Selector[PageState[Request]], snippets: DynamicContent[Request]): Rule =
    req => apply(path, selectors, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent[Request]): Rule =
    req => apply(path, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent[Request], pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(Html5Response(new Html5(req, Selectors.byClassAttr[PageState[Request]])(snippets).resolve(pageLoader(path)))))

  def apply(path: String, selectors: Selectors.type#Selector[PageState[Request]], snippets: DynamicContent[Request], pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(Html5Response(new Html5(req, selectors)(snippets).resolve(pageLoader(path)))))

}

class Html5[T](t:T, selectors: Selectors.type#Selector[PageState[T]])(content: DynamicContent[T]) {
  def resolve(markup: NodeSeq): NodeSeq =
    (for {
      c <- (Template[T](selectors)(content) run markup)(PageState(t, NodeSeq.Empty))
    } yield c._2) getOrElse NodeSeq.Empty
}
