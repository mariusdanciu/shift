package net.shift
package common

object PathUtils {

  def pathToList(path: String) = (path split "/").toList match {
    case "" :: rest => rest
    case e => e
  }

}

object FileSplit {
  def unapply(s: String): Option[(String, String)] = s.split("\\.").toList match {
    case name :: ext :: Nil => Some((name, ext))
    case _ => None
  }
}
