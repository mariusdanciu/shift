package net.shift
package common

import scala.util.Try
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File
import net.shift.io.BinProducer
import net.shift.io.IO._
import net.shift.io.FileSystem

object PathUtils {

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

  def fromPath(path: Path)(implicit fs: FileSystem): Try[BinProducer] = fs reader path

}

object FileUtils {
  def exists(p: Path): Boolean = new File(p.toString()).exists
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
