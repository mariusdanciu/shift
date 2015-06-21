package net.shift
package common

import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.Try
import scala.xml._
import net.shift.io.IO._
import net.shift.io.BinProducer

object XmlAttr {
  def apply(m: MetaData) = XmlImplicits.metaData2Attr(m)
  def apply(name: String, value: String) = new XmlAttr(Map(name -> value))
  def apply() = new XmlAttr(Map.empty)
}

case class XmlAttr(attrs: Map[String, String]) {

  def hasAttr(pair: (String, String)): Boolean = attrs get pair._1 match {
    case Some(value) if (value == pair._2) => true
    case _                                 => false
  }
  def hasAttr(name: String): Boolean = attrs contains name
  def hasClass(name: String): Boolean = attrs.get("class").map(_ contains name) getOrElse false
  def hasId(name: String): Boolean = attrs.get("id").map(_ contains name) getOrElse false

  def toMetaData: MetaData = ((Null: MetaData) /: attrs)((a, e) => a append new UnprefixedAttribute(e._1, e._2, Null))

  def map(f: ((String, String)) => (String, String)): XmlAttr = XmlAttr(attrs.map(f(_)))

  def -(name: String) = XmlAttr(attrs - name)
  def +(name: String, value: String) = XmlAttr(attrs + ((name, value)))
  def get(name: String) = attrs.get(name)
}

object XmlImplicits {
  implicit def metaData2Attr(attrs: MetaData): XmlAttr = XmlAttr(attrs.asAttrMap)
  implicit def attrs2MetaData(attributes: XmlAttr): MetaData =
    ((Null: MetaData) /: attributes.attrs)((acc, attr) => new UnprefixedAttribute(attr._1, attr._2, acc))

  implicit class ElemExt(e: Elem) {
    import XmlUtils._

    def addAttr(name: String, value: String) = e % new UnprefixedAttribute(name, value, Null)
    def removeAttr(name: String) = Xml(e.label, e.attributes remove name, e.child: _*)
    def attr(name: String): Option[String] = e.attributes get name match {
      case Some(Text(t)) => Some(t)
      case _             => None
    }
    def attr(a: (String, String)): Option[String] =
      for (
        ns <- e.attributes.find {
          case PrefixedAttribute(p, k, _, _) => p == a._1 && k == a._2
          case _                             => false
        }
      ) yield ns.value.mkString

    def /(childs: NodeSeq) = Xml(e.label, e.attributes, childs)
  }
}

object Xml {
  import XmlImplicits._
  
  def name(e: Elem) = if (e.prefix == null || e.prefix.isEmpty) 
    e.label
  else
    e.prefix + ":" + e.label
  
  def unapply(e: Elem): Option[(String, XmlAttr, NodeSeq)] =
    Some((name(e), e.attributes, e.child))

  def apply(name: String, attrs: XmlAttr, childs: NodeSeq): Elem = new Elem(null, name, attrs, TopScope, false, childs: _*)
  def apply(name: String, attrs: XmlAttr, childs: Node*): Elem = apply(name, attrs, childs)
  def apply(name: String, attrs: XmlAttr): Elem = apply(name, attrs, NodeSeq.Empty)
  def apply(name: String): Elem = apply(name, XmlAttr(), NodeSeq.Empty: _*)

}

object XmlUtils {
  import XmlImplicits._

  def elemByAttr(e: NodeSeq, attr: (String, String)): Option[Elem] = (e find {
    case x: Elem => !x.attr(attr._1).filter(_ == attr._2).isEmpty
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

  def load(resource: BinProducer): Try[NodeSeq] = toArray(resource).map { arr => XML.load(new java.io.ByteArrayInputStream(arr)) }

  def load(path: Path): Try[NodeSeq] = Try(XML.load(new BufferedInputStream(new FileInputStream(path toString))))

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


