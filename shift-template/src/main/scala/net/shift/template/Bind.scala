package net.shift
package template

import scala.xml._
import MetaData._

case class Attributes(attrs: Map[String, String]) {
  def hasAttr(name: String): Boolean = attrs contains name
  def hasClass(name: String): Boolean = attrs.get("class").map(_ contains name) getOrElse false
}

case class BindMeta(attrs: Attributes, children: NodeSeq)
case class ToBind(name: String, meta: BindMeta)

trait Binds {

  implicit def metaData2Map(attrs: MetaData): Attributes = Attributes(attrs.asAttrMap)
  implicit def attrs2MetaData(attributes: Attributes): MetaData =
    ((Null: MetaData) /: attributes.attrs)((acc, attr) => new UnprefixedAttribute(attr._1, attr._2, acc))

  implicit def elem2ToBind(el: Node): ToBind = ToBind(el.label, BindMeta(el.attributes, el.child))

  def nameOf(e: Elem) = if (e.prefix == null) e.label else e.prefix.mkString + ":" + e.label

  def bind(xml: NodeSeq)(bindFunc: PartialFunction[ToBind, NodeSeq]): NodeSeq = {
    xml flatMap { n =>
      n match {
        case Group(nodes) => bind(nodes)(bindFunc)
        case el: Elem =>
          val v = (applyPf(ToBind(nameOf(el), BindMeta(el.attributes, el.child)))(bindFunc) getOrElse el)
          v match {
            case e: Elem =>
              new Elem(e.prefix, e.label, e.attributes, e.scope, bind(e.child)(bindFunc): _*)
            case n =>
              bind(n)(bindFunc)
          }
        case e => e
      }
    }
  }

  def applyPf[A, B](a: A)(pf: PartialFunction[A, B]): Option[B] = {
    if (pf.isDefinedAt(a)) Some(pf(a)) else None
  }

}

object Binds extends Binds {
  val > = ToBind
  val / = BindMeta
}