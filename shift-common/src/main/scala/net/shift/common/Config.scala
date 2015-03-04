package net.shift
package common

import java.util.Properties
import java.io.ByteArrayInputStream
import scala.collection.JavaConverters._
import PathUtils._
import StringUtils._
import net.shift.io.IO._
import net.shift.io.FileSystem

object Config {

  private var configs: Map[String, String] = Map.empty;

  def load(profile: String = "")(implicit fs: FileSystem) {
    for {
      in <- fromPath(Path(s"config/config$profile.properties"))
      arr <- toArray(in)
    } {
      val p = new Properties();
      p.load(new ByteArrayInputStream(arr))
      configs ++= p.asScala
    }
  }

  def int(p: String, d: Int = 0): Int = configs.get(p).flatMap(toInt(_)).getOrElse(d)

  def long(p: String, d: Long = 0): Long = configs.get(p).flatMap(toLong(_)).getOrElse(d)

  def bool(p: String, d: Boolean = false): Boolean = configs.get(p).flatMap(toBool(_)).getOrElse(d)

  def double(p: String, d: Double = 0.0): Double = configs.get(p).flatMap(toDouble(_)).getOrElse(d)

  def string(p: String, d: String = ""): String = configs.get(p).getOrElse(d)

  def list(p: String, d: List[String] = Nil): List[String] = configs.get(p).map(s => s.trim.split("\\s*,\\s*").toList).getOrElse(d)

}