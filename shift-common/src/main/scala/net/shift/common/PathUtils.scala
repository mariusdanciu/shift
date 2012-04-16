package net.shift
package common

object PathUtils {

  def pathToList(path : String) = (path split "/").toList match {
    case "" :: rest => rest
    case e => e
  }
  
}
