package net.shift
package util

import scala.xml._
import parsing._

trait RenderRule {
  def emptyNonShortNodes: List[String]
  def stripComment: Boolean = false
}

object IeRules extends RenderRule {
  def emptyNonShortNodes = "br" :: "th" :: Nil

}

object NonIeRules extends RenderRule {
  def emptyNonShortNodes = "base" :: "meta" :: 
                           "link" :: "hr" :: "br" ::
                           "param" :: "img" :: "area" :: "input" :: "col" :: Nil
  
}

object XmlUtil {

  def escape(str: String): String = str

  def stringify(node: NodeSeq)(implicit rule: RenderRule): String = {

  def stringify(node: NodeSeq, currentDefNs: NamespaceBinding)(implicit rule: RenderRule): String = node.flatMap(_ match {
    case d: Document => stringify(d.children, TopScope)
    case e: Group => stringify(e.nodes, e scope)
    case c: Comment if !rule.stripComment => c.toString
    case u: Unparsed => u.toString
    case p: PCData => p.toString
    case Elem(prefix, label, attrs, ns, childs @ _*) if (childs == null || childs.isEmpty && 
      !rule.emptyNonShortNodes.contains(label)) => 
      val nms = if (ns != currentDefNs) ns.buildString(currentDefNs) + " " else ""
      "<" + (if (prefix == null) label else prefix + ":" + label) + nms + attrs + " />"
    case Elem(prefix, label, attrs, ns, childs @ _*) => 
      val nms = if (ns != currentDefNs) ns.buildString(currentDefNs) + " " else ""
      "<" + (if (prefix == null) label else prefix + ":" + label) + nms + attrs + ">" + 
      stringify(childs, ns) +
      "</" + (if (prefix == null) label else prefix + ":" + label) + ">"
    case Text(str)  => str 
    case e => ""
  }).toList.mkString

  stringify(node, TopScope)(rule)

  }
}

