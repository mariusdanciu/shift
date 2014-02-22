package net.shift
package common

import scala.util.Try
import scalax.io.Resource
import scalax.io.Input
import java.io.BufferedInputStream
import java.io.FileInputStream

trait PathUtils {

  def pathToList(path: String) = (path split "/").toList match {
    case "" :: rest => rest
    case e => e
  }

  def fromPath(path: Path): Try[Input] =
    Try(Resource.fromInputStream(new BufferedInputStream(new FileInputStream(path toString))))

}

object FileSplit {
  def unapply(s: String): Option[(String, String)] = s.split("\\.").toList match {
    case name :: ext :: Nil => Some((name, ext))
    case _ => None
  }
}
