package net.shift
package template

import scala.util.Success
import scala.util.Try
import scala.xml._
import scala.xml.MetaData._
import net.shift.common.Xml
import net.shift.common.Attributes
import net.shift.common.XmlImplicits._

object Binds {

  def nameOf(e: Elem) = if (e.prefix == null) e.label else e.prefix.mkString + ":" + e.label

  def bind(xml: NodeSeq)(bindFunc: PartialFunction[Elem, NodeSeq]): Try[NodeSeq] = {
    def _bind(xml: NodeSeq): NodeSeq = {
      xml flatMap {
        case Group(nodes) => _bind(nodes)
        case el: Elem =>
          val v = (applyPf(el)(bindFunc) getOrElse el)
          v match {
            case e: Elem =>
              new Elem(e.prefix, e.label, e.attributes, e.scope, _bind(e.child): _*)
            case n =>
              _bind(n)
          }
        case e => e
      }
    }
    Success(_bind(xml))
  }

  def applyPf[A, B](a: A)(pf: PartialFunction[A, B]): Option[B] = {
    if (pf.isDefinedAt(a)) Some(pf(a)) else None
  }

}

object Attrs {
  def unapplySeq(a: Attributes): Option[Seq[(String, String)]] = {
    Some(a.attrs.toList.sorted)
  }
}

object HasClass {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("class") map (v => (v, a))
  def unapply(a: Elem): Option[(String, Attributes)] = a.attributes.attrs.get("class") map (v => (v, a.attributes))
}

object HasClasses {
  def unapply(a: Attributes): Option[(List[String], Attributes)] = a.attrs.get("class") map (v => (v.trim.split("\\s+").toList, a))
  def unapply(a: Elem): Option[(List[String], Attributes)] = a.attributes.attrs.get("class") map (v => (v.trim.split("\\s+").toList, a.attributes))
}

object HasName {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("name") map (v => (v, a))
  def unapply(a: Elem): Option[(String, Attributes)] = a.attributes.attrs.get("name") map (v => (v, a.attributes))
}

object HasValue {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("value") map (v => (v, a))
  def unapply(a: Elem): Option[(String, Attributes)] = a.attributes.attrs.get("value") map (v => (v, a.attributes))
}

object HasId {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("id") map (v => (v, a))
  def unapply(a: Elem): Option[(String, Attributes)] = a.attributes.attrs.get("id") map (v => (v, a.attributes))
}
