package net.shift
package template

import scala.xml._

case class BindMeta(attrs: List[(String, String)], children: NodeSeq)
case class ToBind(name: String, meta: BindMeta)

trait Binds {

  def bind(xml: NodeSeq)(bindFunc: PartialFunction[ToBind, NodeSeq]): NodeSeq = {
    xml flatMap {
      case Group(nodes) => bind(nodes)(bindFunc)
      case e @ Elem(prefix, label, attrs, ns, childs @ _*) =>
        (applyPf(ToBind(label, BindMeta(attrs, childs)))(bindFunc) getOrElse e) match {
          case e: Elem =>
            new Elem(e.prefix, e.label, e.attributes, e.scope, bind(e.child)(bindFunc): _*)
        }
      case e => e
    }
  }

  implicit def metaData2List(attrs: MetaData): List[(String, String)] = attrs.asAttrMap.toList
  implicit def list2MetaDAta(attrs: List[(String, String)]): MetaData =
    ((Null: MetaData) /: attrs)((acc, attr) => new UnprefixedAttribute(attr._1, attr._2, acc))

  def applyPf[A, B](a: A)(pf: PartialFunction[A, B]): Option[B] = {
    if (pf.isDefinedAt(a)) Some(pf(a)) else None
  }

}

object Binds extends Binds {
  val > = ToBind
  val / = BindMeta
}