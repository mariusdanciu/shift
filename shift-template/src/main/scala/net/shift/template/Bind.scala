package net.shift
package template

import scala.xml._
import MetaData._
import scala.util.Try
import scala.util.Success

case class Attributes(attrs: Map[String, String]) {
  def hasAttr(name: String): Boolean = attrs contains name
  def hasClass(name: String): Boolean = attrs.get("class").map(_ contains name) getOrElse false
  def hasId(name: String): Boolean = attrs.get("id").map(_ contains name) getOrElse false
}

case class BindMeta(attrs: Attributes, children: NodeSeq)
case class ToBind(name: String, meta: BindMeta)

trait Binds {

  implicit def metaData2Map(attrs: MetaData): Attributes = Attributes(attrs.asAttrMap)
  implicit def attrs2MetaData(attributes: Attributes): MetaData =
    ((Null: MetaData) /: attributes.attrs)((acc, attr) => new UnprefixedAttribute(attr._1, attr._2, acc))

  implicit def elem2ToBind(el: Node): ToBind = ToBind(el.label, BindMeta(el.attributes, el.child))

  def nameOf(e: Elem) = if (e.prefix == null) e.label else e.prefix.mkString + ":" + e.label

  def bind(xml: NodeSeq)(bindFunc: PartialFunction[ToBind, NodeSeq]): Try[NodeSeq] = {
    def _bind(xml: NodeSeq): NodeSeq = {
      xml flatMap {
        case Group(nodes) => _bind(nodes)
        case el: Elem =>
          val v = (applyPf(ToBind(nameOf(el), BindMeta(el.attributes, el.child)))(bindFunc) getOrElse el)
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

object Binds extends Binds {
  val attributes = ToBind
  val / = BindMeta
}

object Attrs {
  def unapplySeq(a: Attributes): Option[Seq[(String, String)]] = {
    Some(a.attrs.toList.sorted)
  }
}

object HasClass {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("class") map (v => (v, a))
  def unapply(a: ToBind): Option[(String, Attributes)] = a.meta.attrs.attrs.get("class") map (v => (v, a.meta.attrs))
  def unapply(a: BindMeta): Option[(String, Attributes)] = a.attrs.attrs.get("class") map (v => (v, a.attrs))
}

object HasClasses {
  def unapply(a: Attributes): Option[(List[String], Attributes)] = a.attrs.get("class") map (v => (v.trim.split("\\s+").toList, a))
  def unapply(a: ToBind): Option[(List[String], Attributes)] = a.meta.attrs.attrs.get("class") map (v => (v.trim.split("\\s+").toList, a.meta.attrs))
  def unapply(a: BindMeta): Option[(List[String], Attributes)] = a.attrs.attrs.get("class") map (v => (v.trim.split("\\s+").toList, a.attrs))
}

object HasName  {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("name") map (v => (v, a))
  def unapply(a: ToBind): Option[(String, Attributes)] = a.meta.attrs.attrs.get("name") map (v => (v, a.meta.attrs))
  def unapply(a: BindMeta): Option[(String, Attributes)] = a.attrs.attrs.get("name") map (v => (v, a.attrs))
}

object HasId {
  def unapply(a: Attributes): Option[(String, Attributes)] = a.attrs.get("id") map (v => (v, a))
  def unapply(a: ToBind): Option[(String, Attributes)] = a.meta.attrs.attrs.get("id") map (v => (v, a.meta.attrs))
  def unapply(a: BindMeta): Option[(String, Attributes)] = a.attrs.attrs.get("id") map (v => (v, a.attrs))
}
