package net.shift
package common

import scala.util.Try
import scalax.io.Resource
import scalax.io.Input
import java.io.BufferedInputStream
import java.io.FileInputStream

trait PathUtils {

  def pathToList(path: String) = {
    val l = (path split "/").toList match {
      case "" :: rest => rest
      case e          => e
    }
    if (path.endsWith("/"))
      l ::: List("")
    else
      l
  }
  def fromPath(path: Path): Try[Input] =
    Try(Resource.fromInputStream(new BufferedInputStream(new FileInputStream(path toString))))

}

object FileSplit {

  def unapply(s: Path): Option[(String, String)] = unapply(s.toString())

  def unapply(s: String): Option[(String, String)] = {
    val pos = s.lastIndexOf(".")
    if (pos > -1) {
      val t = s.splitAt(pos + 1)
      Some((t._1.dropRight(1), t._2))
    } else None
  }

}
