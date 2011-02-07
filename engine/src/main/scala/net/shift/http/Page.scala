package net.shift
package http

import scala.xml._
import app.Application._

trait PageProp {
  def & (prop: PageProp): PageProps = PageProps(this :: List(prop))
}

case class PageProps(props: List[PageProp]) extends PageProp

case object Hidden extends PageProp
case object HiddenIfNoChildren extends PageProp
case class Selected(node: (Page) => NodeSeq) extends PageProp
case class Access(func: AccessControlFunc) extends PageProp



class Arrow(path: String) {
  def <<(resource: String) = Page(path, resource, None)
}

object Page {

  implicit def str2Arrow(name: String): Arrow = new Arrow(name)
}

case class Page(path: String, 
		resource: String, 
		props: Option[PageProps], 
		pages: Page*) {
  import Page._

  def having(prop: PageProps): Page = new Page(resource, path, Some(prop))
  def parentOf(childs: Pages): Page = new Page(resource, path, props, childs.pages: _*)
  
  def & (page: Page): Pages = new Pages(List(this, page))

  def hasChilds: Boolean = pages.length > 0

}

case class Pages(val pages: List[Page]) {

  def & (page: Page): Pages = new Pages(pages :+ page)
  
  def & (nextPages: List[Page]): Pages = new Pages(pages ::: nextPages)

}

case class SiteMap(val pages: Pages) {

}



object Main {

  def main(args: Array[String]) = {
     import Page._
  
     val siteMap = SiteMap(
      ("/pages/homePage" << "/" parentOf
         "/pages/producs" << "/products" &
         "/pages/help" << "/help") &
      ("/pages/error" << "/error"))

     println(siteMap)

  }
}

