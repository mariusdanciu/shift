package net.shift
package common

import scala.xml._

object XmlUtils {

  def attribute(e: Elem, name: String): Option[String] = e.attributes.get(name).map( _ mkString)
  
  def attribute(e: Elem, prefix: String, name: String): Option[String] = 
    for (ns <- e.attributes.find{
      case PrefixedAttribute(p, k, _, _) => p == prefix && k == name
      case _ => false
    }) yield ns.value.mkString

  def elemByAttr(e: NodeSeq, attr: (String, String)) : Option[Elem] = (e find {
    case x: Elem => !attribute(x, attr._1).filter(_ == attr._2).isEmpty
    case _ => false
  }) match {
    case Some(e: Elem) => Some(e)
    case _ => None
  }

}
