package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._

object XhtmlPage {

  type PageStateType = String

  def apply(path: String, selectors: Selectors.type#Selector[PageState[PageStateType]], snippets: DynamicContent[PageStateType]): Rule =
    req => apply(path, selectors, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent[PageStateType]): Rule =
    req => apply(path, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent[PageStateType], pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(XhtmlResponse(new XhtmlPage(Selectors.byClassAttr[PageState[PageStateType]])(snippets).resolve(pageLoader(path)))))

  def apply(path: String, selectors: Selectors.type#Selector[PageState[PageStateType]], snippets: DynamicContent[PageStateType], pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(XhtmlResponse(new XhtmlPage(selectors)(snippets).resolve(pageLoader(path)))))

}

class XhtmlPage(selectors: Selectors.type#Selector[PageState[XhtmlPage.PageStateType]])(content: DynamicContent[XhtmlPage.PageStateType]) {
  def resolve(markup: NodeSeq): NodeSeq =
    (for {
      c <- (Template[XhtmlPage.PageStateType](selectors)(content) run markup)(PageState("start", NodeSeq.Empty))
    } yield c._2) getOrElse NodeSeq.Empty
}
