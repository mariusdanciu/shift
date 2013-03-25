package net.shift
package engine
package page

import scala.xml.NodeSeq
import template._
import http._
import common._

object XhtmlPage {

  def apply(path: String, selectors: List[Selectors.type#Selector], snippets: DynamicContent): Rule =
    req => apply(path, selectors, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent): Rule =
    req => apply(path, snippets, (p: String) => XmlUtils.load(req.resource(p)))(req)

  def apply(path: String, snippets: DynamicContent, pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(XhtmlResponse(new XhtmlPage(List(Selectors.byClassAttr), snippets).resolve(pageLoader(path)))))

  def apply(path: String, selectors: List[Selectors.type#Selector], snippets: DynamicContent, pageLoader: String => NodeSeq): Rule =
    req => Some(resp => resp(XhtmlResponse(new XhtmlPage(selectors, snippets).resolve(pageLoader(path)))))

}

class XhtmlPage(selectors: List[Selectors.type#Selector], content: DynamicContent) {
  def resolve(markup: NodeSeq): NodeSeq = Template(selectors)(content) run markup
}
