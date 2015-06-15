package net.shift
package template

import scala.xml.Elem
import scala.xml.NodeSeq

import common.XmlImplicits._

trait TemplateUtils {
  object IdAttr {
    def unapply(e: Elem): Option[String] = for {
      wrp <- e attr "id"
    } yield wrp
  }

  object Head {
    def unapply(n: NodeSeq): Option[NodeSeq] = n match {
      case Children("head", childs) => Some(childs)
      case _                        => None
    }
  }

  object Children {
    def unapply(n: NodeSeq): Option[(String, NodeSeq)] = n match {
      case e: Elem => Some((e.label, NodeSeq.fromSeq(e.child)))
      case e =>
        val res = (n map {
          case e: Elem => Some((e.label, NodeSeq.fromSeq(e.child)))
          case _       => None
        })
        if (res.isEmpty) None else res.head
    }
  }

}