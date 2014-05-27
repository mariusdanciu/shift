package net.shift
package template

import scala.xml.NodeSeq
import net.shift.common.XmlUtils
import scala.xml.Elem
import scala.xml.Node

trait TemplateUtils {
  object IdAttr extends XmlUtils {
    def unapply(e: Elem): Option[String] = for {
      wrp <- attribute(e, "id")
    } yield wrp
  }

  object Head {
    def unapply(n: NodeSeq): Option[NodeSeq] = n match {
      case Children("head", childs) => Some(childs)
      case _ => None
    }
  }

  object Children {
    def unapply(n: NodeSeq): Option[(String, NodeSeq)] = n match {
      case e: Elem => Some((e.label, NodeSeq.fromSeq(e.child)))
      case e =>
        val res = (n map {
          case e: Elem => Some((e.label, NodeSeq.fromSeq(e.child)))
          case _ => None
        })
        if (res.isEmpty) None else res.head
    }
  }

}