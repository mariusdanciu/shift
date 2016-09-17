package net.shift
package loc

import java.io.FileInputStream
import scala.util.Try
import scala.util.control.Exception._
import org.json4s._
import org.json4s.native.JsonMethods._
import io.IO._
import net.shift.common.Path
import net.shift.io.FileSystem

object Loc {

  private var cache: Map[Language, Map[String, LocEntry]] = Map.empty

  var location = "localization/"

  implicit val formats = DefaultFormats

  private def stringify(p: Path, l: Language)(implicit fs: FileSystem) = for {
    prod <- fs reader p
    arr <- producerToArray(prod)
  } yield { new String(arr, "utf-8") }

  private def load(name: String, l: Language)(implicit fs: FileSystem) {

    val t1 = stringify(Path(s"$location$name${l}.json"), l)

    val t2 = stringify(Path(s"$location$name${l.name}.json"), l)

    (t1 orElse t2) map { s =>
      val entries = (parse(s).extract[List[LocEntry]]).map(e => (e.name, e)).toMap
      cache = cache + (l -> entries)
    }

  }

  def loc0(prefix: String, l: Language)(name: String)(implicit fs: FileSystem): Text = loc(prefix, l)(name, Nil)

  def loc(prefix: String, l: Language)(name: String, params: Seq[String])(implicit fs: FileSystem): Text = {
    val pref = if (prefix == null) "" else prefix + "_"
    (for {
      m <- cache.get(l) orElse { load(s"${pref}", l); cache.get(l) }
      e <- m.get(name)
    } yield {
      Text(e.code, e.text.format(params: _*))
    }) getOrElse Text("0", "???")
  }

  def loc(l: Language)(name: String, params: Seq[String])(implicit fs: FileSystem): Text = loc(null, l)(name, params)

  def loc0(l: Language)(name: String)(implicit fs: FileSystem): Text = loc(null, l)(name, Nil)

}

case class LocEntry(code: String, name: String, text: String)
case class Text(code: String, text: String)

case class Language(name: String, country: Option[String] = None, variant: Option[String] = None) {
  override def toString = {
    (country, variant) match {
      case (Some(c), Some(v)) => s"${name}_${c}_${v}"
      case (Some(c), None)    => s"${name}_${c}"
      case (None, Some(v))    => s"${name}__${v}"
      case (None, None)       => s"${name}"
    }
  }

  def toHttpString = {
    country match {
      case Some(c) => s"${name}_${c}"
      case None    => s"${name}"
    }
  }

}
