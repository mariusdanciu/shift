package net.shift
package util

import scala.xml._
import parsing._

trait RenderRule {
  def emptyNonShortNodes: List[String]
  def stripComment: Boolean = false
}

class IeRules extends RenderRule {
  def emptyNonShortNodes = "br" :: "th" :: Nil

}

class NonIeRules extends RenderRule {
  def emptyNonShortNodes = "base" :: "meta" :: 
                           "link" :: "hr" :: "br" ::
                           "param" :: "img" :: "area" :: "input" :: "col" :: Nil
  
}

object XmlUtil {

  def escape(str: String): String = str

  def stringify(node: NodeSeq)(implicit rule: RenderRule): String = node.flatMap(_ match {
    case d: Document => stringify(d.children)
    case e: Group => stringify(e.nodes)
    case c: Comment if !rule.stripComment => c.toString
    case u: Unparsed => u.toString
    case p: PCData => p.toString
    case Elem(prefix, label, attrs, ns, childs @ _*) if (childs == null || childs.isEmpty && 
      !rule.emptyNonShortNodes.contains(label)) => 
      "<" + (if (prefix == null) label else prefix + ":" + label) + attrs + " />"
    case Elem(prefix, label, attrs, ns, childs @ _*) => 
      "<" + (if (prefix == null) label else prefix + ":" + label) + attrs + ">" + 
      stringify(childs) +
      "</" + (if (prefix == null) label else prefix + ":" + label) + ">"
    case Text(str)  => str 
    case e => ""
  }).toList.mkString

}

