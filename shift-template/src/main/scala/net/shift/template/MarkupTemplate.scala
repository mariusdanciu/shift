package net.shift
package template

import scala.xml._


object MarkupTemplate extends App {
  
  val s = "<script></script>"
  val xml = <html x:version="5"><head></head><body>{s}<br></br>

  <div id="name" class="bold mysnippet">
  </div>

  </body></html>

  val snippets: Map[String, NodeSeq => NodeSeq] = Map(("mysnippet", e => <span>hi</span>))
  println(new MarkupTemplate(false, List(ClassNodeSelector(snippets))).build(
    xml
  ))

}

trait NodeSelector extends (NodeSeq => NodeSeq)

case class ClassNodeSelector(snippets: Map[String, NodeSeq => NodeSeq]) extends NodeSelector {
  def apply(in: NodeSeq) : NodeSeq = in match {
    case e : Elem => 
      (NodeSeq.Empty /: ((for { cls <- e.attributes.get("class").toList
			       name <- ("\\s+".r split (cls toString)).toList if (snippets.contains(name))
				 snippet <- snippets.get(name)
			     } yield snippet(e)).toList match {
			       case Nil => e
			       case e => e
			     }))((l, r) => l ++ r)

    case _ => Nil
  }
}



case class MarkupTemplate(stripComments: Boolean, 
			  selectors: List[NodeSelector]) {

  private def stringify(e: Node): String = {
    val sb = new StringBuilder()
    val name = e.nameToString(sb).toString
    if (e.attributes ne null) e.attributes.buildString(sb)
    e.scope.buildString(sb, e.scope)
    "<" + sb + ">" + build(e.child) + "</" + name + ">"
  }

  def build(nodes: NodeSeq): String = (nodes flatMap { 
    case Group(childs) => build(childs)
    case Text(str) => escape(str)
    case e : Unparsed => e mkString
    case e : PCData => e mkString
    case e : Atom[_] => escape(e.data.toString)
    case e : Comment => e mkString
    case e : Elem => 
      (for (s <- selectors; node <- s(e)) yield stringify(node)) mkString
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
