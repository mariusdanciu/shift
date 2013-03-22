package net.shift
package template

import scala.xml._
import common.XmlUtils._


object Selectors {

  type Selector = Map[String, SnippetFunc] => NodeSeq => Option[NodeSeq]

  val bySnippetAttr: Selector = snippets => in => in match {
    case e: Elem =>
      for (
        value <- attribute(e, "shift", "snippet");
        snippet <- snippets.get(value)
      ) yield snippet(e)
    case _ => None
  }

  /**
   * Extracts the node class attribute and looks for snippets mathing the class names
   */
  val byClassAttr: Selector = snippets => in => in match {
    case e: Elem => (for (
      value <- attribute(e, "class").toList;
      cls <- ("\\s+".r split value) if snippets.contains(cls)
    ) yield cls) match {
      case Nil => None
      case l => Some((NodeSeq.Empty /: (l.map(n => snippets(n)(e))))(_ ++ _))
    }
    case _ => None
  }

  /**
   * Extracts the node id attribute and looks for snippets matching the node id
   */
  val byIdAttr: Selector = snippets => in => in match {
    case e: Elem => for (
      node <- attribute(e, "id");
      snippet <- snippets.get(node mkString)
    ) yield snippet(e)
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

  def apply(selectors: List[Selectors.type#Selector])(snippets: DynamicContent) =
    new Template(selectors, snippets)
  
  /**
   * Returns the String representation of the 'nodes'
   *
   */
  def mkString(nodes: NodeSeq): String = (nodes flatMap {
    case Group(childs) => mkString(childs)
    case Text(str) => escape(str)
    case e: Unparsed => e mkString
    case e: PCData => e mkString
    case e: Atom[_] => escape(e.data.toString)
    case e: Comment => e mkString
    case e: Elem => {
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
 * Analyze the page nodes and invokes the selectors which runs the corresponding snippets
 *
 */
class Template(selectors: List[Selectors.type#Selector], snippets: DynamicContent) {

  private val snippetsMap = snippets toMap
  
  /**
   * Runs the in template and produces the modified template
   *
   */
  def run(in: NodeSeq): NodeSeq = in flatMap {
    case Group(childs) => run(childs)

    case c: Comment => (for (s <- selectors) yield s(snippetsMap)(c)).filter(n => !n.isEmpty) match {
      case Nil => c
      case l => for (optNodeSeq <- l; ns <- optNodeSeq.toList; n <- ns) yield n
    }

    case e: Elem => (for (s <- selectors) yield s(snippetsMap)(e)).filter(n => !n.isEmpty) match {
      case Nil => new Elem(e.prefix, e.label, e.attributes, e.scope, run(e.child): _*)
      case l => run(for (optNodeSeq <- l; ns <- optNodeSeq.toList; n <- ns) yield n)
    }
    case e => e
  }

}

