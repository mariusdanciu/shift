package net.shift
package template

import scala.xml._
import common.XmlUtils._

object TemplateMain extends App {
  import Template._
  import Selectors._
  
  val s = "<script></script>"
  val xml = <html><head></head><body>{s}<br></br>

  <div id="name" class="bold mysnippet">
  </div>

  <!-- test 1-->
  <div shift:snippet="name"></div>
  
  <!-- test 2-->

  </body></html>

  val snippets: Map[String, NodeSeq => NodeSeq] = Map (
    ("name", e => <div> got name <span class="mysnippet">hi name</span></div>), 
    ("mysnippet", e => <p>hi my snippet</p>)
  )
  println(mkString(Template(snippets, List(bySnippetAttr, stripComments)).run(xml)))

  val e: Elem = <div shift:snippet="marius"></div>
  println(attribute(e, "shift", "snippet"))
}

object Selectors {

  type Selector = Map[String, NodeSeq => NodeSeq] => NodeSeq => Option[NodeSeq]

  val bySnippetAttr : Selector = snippets => in => in match {
    case e : Elem =>
      for(value <- attribute(e, "shift", "snippet");
	  snippet <- snippets.get(value)) yield snippet(e)
    case _ => None
  }
  
  /** 
   * Extracts the node class attribute and looks for snippets mathing the class names
   */
  val byClassAttr: Selector = snippets => in => in match {
    case e : Elem => (for(value <- attribute(e, "class").toList;
			  cls <- ("\\s+".r split value) if snippets.contains(cls)) yield cls) match {
			    case Nil => None
			    case l => Some((NodeSeq.Empty /: (l.map(n => snippets(n)(e))))(_ ++ _))
			  }
    case _ => None
  }

  /** 
   * Extracts the node id attribute and looks for snippets mathing the node id
   */
  val byIdAttr: Selector = snippets => in => in match {
    case e : Elem => for(node <- attribute(e, "id");
			 snippet <- snippets.get(node mkString)) yield snippet(e)
    case _ => None
  }

  /** 
   * Removes the XML comments
   */
  val stripComments: Selector = snippets => in => in match {
    case c: Comment => Some(NodeSeq.Empty)
    case _ => None
  }


}

object Template {

  /** 
   * Returns the String representation of the 'nodes'
   *   
   */
  def mkString(nodes: NodeSeq): String = (nodes flatMap { 
    case Group(childs) => mkString(childs)
    case Text(str) => escape(str)
    case e : Unparsed => e mkString
    case e : PCData => e mkString
    case e : Atom[_] => escape(e.data.toString)
    case e : Comment => e mkString
    case e : Elem => {
      val name = if (e.prefix eq null) e.label else e.prefix + ":" + e.label
      val attrs = if (e.attributes ne null) e.attributes.toString
      "<" + name + attrs + ">" + mkString(e.child) + "</" + name + ">"
    }
    case k => k.getClass toString
  }) mkString

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

/** 
 * Template
 * 
 * Analyses the page nodes and invokes the selectors which runs the corresponding snippets 
 *   
 */
case class Template(snippets: Map[String, NodeSeq => NodeSeq], selectors: List[Selectors.type#Selector]) {

  /** 
   * Runs the in template and produces the modified template
   *   
   */
  def run(in: NodeSeq) : NodeSeq = in flatMap {
    case Group(childs) => run(childs)

    case c: Comment => (for (s <- selectors) yield s(snippets)(c)).filter(n => !n.isEmpty) match {
      case Nil => c
      case l => for (optNodeSeq <- l; ns <- optNodeSeq.toList; n <- ns ) yield n
    }

    case e: Elem => (for (s <- selectors) yield s(snippets)(e)).filter(n => !n.isEmpty) match {
      case Nil => new Elem(e.prefix, e.label, e.attributes, e.scope, run(e.child):_*)
      case l => run(for (optNodeSeq <- l; ns <- optNodeSeq.toList; n <- ns ) yield n)
    }
    case e => e
  }



}
