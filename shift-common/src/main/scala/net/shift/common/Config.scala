package net.shift
package common

import java.util.Properties
import java.io.ByteArrayInputStream
import scala.collection.JavaConverters._
import StringUtils._
import net.shift.io.IO._
import net.shift.io.FileSystem
import scala.util.Try

object Config {
  def load(profile: String = "")(implicit fs: FileSystem): Try[Config] = {
    for {
      in <- fs.reader(Path(s"config/config$profile.properties"))
      arr <- toArray(in)
    } yield {
      val p = new Properties();
      p.load(new ByteArrayInputStream(arr))
      new {
        override val configs = p.asScala.toMap
      } with Config()
    }

  }

}

class Config() { self =>

  protected val configs: Map[String, String] = Map.empty

  def append(other: Map[String, String]): Config = new {
    val config = self.configs ++ other
  } with Config()

  def int(p: String, d: Int = 0): Int = configs.get(p).flatMap(toInt(_)).getOrElse(d)

  def long(p: String, d: Long = 0): Long = configs.get(p).flatMap(toLong(_)).getOrElse(d)

  def bool(p: String, d: Boolean = false): Boolean = configs.get(p).flatMap(toBool(_)).getOrElse(d)

  def double(p: String, d: Double = 0.0): Double = configs.get(p).flatMap(toDouble(_)).getOrElse(d)

  def string(p: String, d: String = ""): String = configs.get(p).getOrElse(d)

  def list(p: String, d: List[String] = Nil): List[String] = configs.get(p).map(s => s.trim.split("\\s*,\\s*").toList).getOrElse(d)

}