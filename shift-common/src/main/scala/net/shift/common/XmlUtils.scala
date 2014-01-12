package net.shift
package common

import scala.xml._
import io._
import java.io.Reader
import scalax.io._
import java.io.FileInputStream
import scala.util.Try

object XmlUtils {

  implicit def elem2NodeOps(e: Elem): NodeOps = new NodeOps(e)

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

  def load(resource: Input): Try[NodeSeq] = 
    Try(XML.load(new java.io.ByteArrayInputStream(resource.byteArray)))

  def load(path: Path): Try[NodeSeq] = 
    Try(XML.load(new java.io.ByteArrayInputStream(Resource.fromInputStream(new FileInputStream(path.toString)).byteArray)))

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

case class NodeOps(e: Elem) {

  def attr(name: String, value: String): NodeOps = new NodeOps(e % new UnprefixedAttribute(name, value, Null))

  def removeAttr(name: String): NodeOps = new NodeOps(new Elem(e.prefix, e.label, e.attributes.remove(name), e.scope, e.child: _*))

}
