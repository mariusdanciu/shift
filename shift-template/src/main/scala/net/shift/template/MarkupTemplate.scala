package net.shift
package template

import scala.xml._


object MarkupTemplate extends App {
  
  val s = "<script></script>"
  val xml = <html x:version="5"><head></head><body>{s}<br></br></body></html>
  println(xml)
  println(new MarkupTemplate(false, Set("br")).build(
    xml
  ))

}

case class MarkupTemplate(stripComments: Boolean, inlineNodes: Set[String]) {

  def build(nodes: NodeSeq): String = (nodes flatMap { 
    case Group(childs) => build(childs)
    case Text(str) => escape(str)
    case e : Unparsed => e mkString
    case e : PCData => e mkString
    case e : Atom[_] => escape(e.data.toString)
    case e @ Comment(c) => e mkString
    case e @ Elem(pre, name, attrs, scope, childs @ _*) if (! inlineNodes.contains(name))=> {
      val sb = new StringBuilder()
      val name = e.nameToString(sb).toString
      if (e.attributes ne null) attrs.buildString(sb)
      e.scope.buildString(sb, scope)
      "<" + sb + ">" + build(childs) + "</" + name + ">"
    }
    case e @ Elem(pre, name, attrs, scope, childs @ _*) if (inlineNodes contains name)=> {
      val sb = new StringBuilder()
      val name = e.nameToString(sb).toString
      if (e.attributes ne null) attrs.buildString(sb)
      e.scope.buildString(sb, scope)
      "<" + sb + "/>"
    }
    case k => k.getClass toString
  }) mkString("")

  private def escape(str: String): String = {
    val len = str.length
    var pos = 0
    val sb = new StringBuilder()
    while (pos < len) {
      str.charAt(pos) match {
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case '&' => sb.append("&amp;")
        case '"' => sb.append("&quot;")
        case '\n' => sb.append('\n')
        case '\r' => sb.append('\r')
        case '\t' => sb.append('\t')
        case c if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) => sb.append(c)
      }
      pos += 1
    }
    sb toString   
  }

}
