package net.shift
package http

import scala.xml._
import Application._

trait PageProp {
  def & (prop: PageProp): PageProps = PageProps(this :: List(prop))
}

case class PageProps(props: List[PageProp]) extends PageProp

case object Hidden extends PageProp
case object HiddenIfNoChildren extends PageProp
case class Selected(node: (Page) => NodeSeq) extends PageProp
case class Access(func: AccessControlFunc) extends PageProp


object Page {
  implicit def str2Path(path: String): Path = Path(path)
  implicit def str2Page(name: String): Page = Page(name)

  def apply(name: String) = new Page(name, Path.empty, Path.empty, None)
  def apply(name: String, uri: Path, resource: Path) = new Page(name, uri, resource, None)

}

case class Page(name: String, uri: Path, resource: Path, props: Option[PageProp], pages: Page*) {
  import Page._

  def withResource(res: String): Page = new Page(name, uri, str2Path(res), props, pages: _*)
  def mapsUri(path: Path): Page = Page(name, path, Path.empty, None)
  def having(prop: PageProp): Page  = new Page(name, uri, resource, Some(prop))
  def parentOf(childs: Pages): Page  = new Page(name, uri, resource, props, childs.pages: _*)
  
  def & (page: Page): Pages = new Pages(List(this, page))

  def hasChilds: Boolean = pages.length > 0
}

case class Pages(pages: List[Page]) {
  def & (page: Page): Pages = new Pages(pages ::: List(page))
}

case class SiteMap(pages: Pages) {

   def select(path: Path): Option[Page] = {

     def lookup(path: Path, pages: List[Page]): Option[Page] = {
       val matchesFirst = pages match {
         case Nil => false
         case list => path matches list.head.uri
       }

       pages match {
         case head :: _ if (!matchesFirst && head.hasChilds) => lookup(path, head.pages.toList)
         case head :: tail if (!matchesFirst) => lookup(path, tail)
         case head :: _ if (matchesFirst) => Some(head)
         case _ => None
       }
     }

     lookup(path, pages pages)
   }
  
}

object Main {

  def main(args: Array[String]) = {
     import Page._


     val p1 = "home" mapsUri "/a/b/?/d" withResource "home" 
     val p2 = "work" mapsUri "/a/c/*" withResource "home"
     val p3 = "admin" mapsUri "/d/f/*/d" withResource "home"
     val p4 = "users" mapsUri "/w/e/+/d" withResource "home"

     val sm = SiteMap(p1 & (p2 parentOf (p3 & p4)))

     println(Path("/a/c") matches Path("/a/c/*"))
     println(sm select Path("/a/c"))


  }

}

