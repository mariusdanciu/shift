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
  
  def ~ (page: Page): Pages = new Pages(List(this, page))
}

case class Pages(pages: List[Page]) {
  def ~ (page: Page): Pages = new Pages(pages ::: List(page))

}

// temporary code for quick test
object Run {
 
  def main(args: Array[String]){

    import Page._

    val p1 = "home" mapsUri "/home" withResource "/pages/home/index" having Hidden & Selected((page) => <b></b>)
    val p2 = "users" mapsUri "/home/users" withResource "/pages/users/index"
    val p3 = "devs" mapsUri "/home/dev" withResource "/pages/devs/index"
    val p4 = "photos" mapsUri "/home/photos" withResource "/pages/photos/index"
    
    val siteMap = p1 ~ (p2 parentOf (p3 ~ p4))

    siteMap.pages map ( println )
  }


}

