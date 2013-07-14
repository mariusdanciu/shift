package net.shift
package common

import scala.xml._
import io._
import java.io.Reader
import IOUtils._

object XmlUtils {

  def attribute(e: Elem, name: String): Option[String] = e.attributes.get(name).map(_ mkString)

  def attribute(e: Elem, prefix: String, name: String): Option[String] =
    for (
      ns <- e.attributes.find {
        case PrefixedAttribute(p, k, _, _) => p == prefix && k == name
        case _ => false
      }
    ) yield ns.value.mkString

  def elemByAttr(e: NodeSeq, attr: (String, String)): Option[Elem] = (e find {
    case x: Elem => !attribute(x, attr._1).filter(_ == attr._2).isEmpty
    case _ => false
  }) match {
    case Some(e: Elem) => Some(e)
    case _ => None
  }

  def load(resource: ReadChannel): NodeSeq = XML.load(resource)

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

  private def escape(str: String): String = ("" /: str)(_ + escape(_))

  private def escape(c: Char): String = c match {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '&' => "&amp;"
    case '"' => "&quot;"
    case '\n' => "\n"
    case '\r' => "\r"
    case '\t' => "\t"
    case c if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) => c toString
    case _ => ""
  }
  
}
