package net.shift {
package http {

import scala.xml._
import Application._



trait PageProp {

  private var props: List[PageProp] = Nil

  def & (prop: PageProp) = props ::: List(prop)

  def hasProp(prop: PageProp): Boolean = !(props filter (_ == prop) isEmpty)
}

case object Hidden extends PageProp
case object HiddenIfNoChildren extends PageProp
case class Access(func: AccessControlFunc) extends PageProp


object Page {

  def apply(name: String) = new Page(name, Nil, Nil, None)
  def apply(name: String, uri: List[String], resource: List[String]) = new Page(name, uri, resource, None)
}

case class Page(name: String, uri: List[String], resource: List[String], props: Option[PageProp], pages: Page*) {

  def / (part: String) = new Page(name, uri ::: List(part), resource, props, pages: _*)

}

private class ResPage(name: String, uri: List[String], resource: List[String], props: Option[PageProp], pages: Page*) extends Page(name, uri, resource, props, pages: _*) {

  override def / (part: String) = new Page(name, uri, resource ::: List(part), props, pages: _*)

}

}
}
