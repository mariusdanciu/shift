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

}
