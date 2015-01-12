package net.shift
package common

import java.io.FileInputStream

import scala.io._
import scala.util.Try
import scala.xml._

import scalax.io._

trait XmlUtils {

  implicit def elem2NodeOps(e: Elem): NodeOps = new NodeOps(e)
  implicit def nodeOps2Elem(n: NodeOps): Elem = n e

  def attribute(e: Elem, name: String): Option[String] = e.attributes.get(name).map(_ mkString)

  def attribute(e: Elem, prefix: String, name: String): Option[String] =
    for (
      ns <- e.attributes.find {
        case PrefixedAttribute(p, k, _, _) => p == prefix && k == name
        case _                             => false
      }
    ) yield ns.value.mkString

  def elemByAttr(e: NodeSeq, attr: (String, String)): Option[Elem] = (e find {
    case x: Elem => !attribute(x, attr._1).filter(_ == attr._2).isEmpty
    case _       => false
  }) match {
    case Some(e: Elem) => Some(e)
    case _             => None
  }

  def elemByName(n: NodeSeq, name: String): Option[Elem] = {
    n match {
      case Group(g)                     => elemByName(g, name)
      case e: Elem if (e.label == name) => Some(e)
      case e: Elem                      => elemByName(e.child, name)
      case e: Text                      => None
      case e: Comment                   => None
      case e: PCData                    => None
      case e: NodeSeq => ((None: Option[Elem]) /: e)((a, el) => a match {
        case Some(_) => a
        case _       => elemByName(el, name)
      })
      case _ => None
    }
  }
  def load(resource: Input): Try[NodeSeq] =
    Try(XML.load(new java.io.ByteArrayInputStream(resource.byteArray)))

  def load(path: Path): Try[NodeSeq] =
    Try(XML.load(new java.io.ByteArrayInputStream(Resource.fromInputStream(new FileInputStream(path.toString)).byteArray)))

  def load(in: String): Try[NodeSeq] = load(in.getBytes("utf-8"))

  def load(in: Array[Byte]): Try[NodeSeq] = Try(XML.load(new java.io.ByteArrayInputStream(in)))

  /**
   * Returns the String representation of the 'nodes'
   *
   */
  def mkString(nodes: NodeSeq): String = (nodes flatMap {
    case Group(childs) => mkString(childs)
    case Text(str)     => escapeButAmp(str)
    case e: Unparsed   => e mkString
    case e: PCData     => e mkString
    case e: Atom[_]    => escape(e.data.toString)
    case e: Comment    => e mkString
    case e: Elem => {
      val name = if (e.prefix eq null) e.label else e.prefix + ":" + e.label
      val attrs = if (e.attributes ne null) e.attributes.toString
      "<" + name + attrs + ">" + mkString(e.child) + "</" + name + ">"
    }
    case k => k.getClass toString
  }) mkString

  private def escape(str: String): String = ("" /: str)(_ + escape(_))
  private def escapeButAmp(str: String): String = ("" /: str)(_ + escapeButAmp(_))

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

  private def escapeButAmp(c: Char): String = c match {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '\n' => "\n"
    case '\r' => "\r"
    case '\t' => "\t"
    case c if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) => c toString
    case _ => ""
  }

}

object NodeOps {
  def node(name: String): Elem = node(name, Map.empty)

  def node(name: String, attrs: Map[String, String]): Elem = new Elem(null,
    name,
    ((Null: MetaData) /: attrs)((a, e) => MetaData.concatenate(a, new UnprefixedAttribute(e._1, e._2, Null))),
    TopScope,
    NodeSeq.Empty.toSeq: _*)

}

case class NodeOps(e: Elem) {

  def /(ns: NodeSeq): NodeOps = new NodeOps(new Elem(e.prefix, e.label, e.attributes, e.scope, (e.child ++ ns): _*))

  def attr(name: String, value: String): NodeOps = new NodeOps(e % new UnprefixedAttribute(name, value, Null))

  def removeAttr(name: String): NodeOps = new NodeOps(new Elem(e.prefix, e.label, e.attributes.remove(name), e.scope, e.child: _*))

  def getAttr(name: String): Option[String] = e.attributes.get(name).map(_ mkString)

  def getAttr(prefix: String, name: String): Option[String] = for (
    ns <- e.attributes.find {
      case PrefixedAttribute(p, k, _, _) => p == prefix && k == name
      case _                             => false
    }
  ) yield ns.value.mkString
}
