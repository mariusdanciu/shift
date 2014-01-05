package net.shift
package loc

import java.io.FileInputStream

import scala.util.Try
import scala.util.control.Exception._

import org.json4s._
import org.json4s.native.JsonMethods._

import scalax.io.Resource

object Loc {

  private var cache: Map[Language, Map[String, LocEntry]] = Map.empty

  var location = "localization/"

  implicit val formats = DefaultFormats

  private def load(name: String, l: Language) {

    val t1 = Try {
      Resource.fromInputStream(new FileInputStream(s"$location$name${l}.json")).string
    }

    val t2 = Try {
      Resource.fromInputStream(new FileInputStream(s"$location$name${l.language}.json")).string
    }

    (t1 orElse t2) map { s =>
      val entries = (parse(s).extract[List[LocEntry]]).map(e => (e.name, e)).toMap
      cache = cache + (l -> entries)
    }

  }

  def loc0(prefix: String, l: Language)(name: String): Text = loc(prefix, l)(name, Nil)

  def loc(prefix: String, l: Language)(name: String, params: Seq[String]): Text = {
    val pref = if (prefix == null) "" else prefix + "_"
    (for {
      m <- cache.get(l) orElse { load(s"${pref}", l); cache.get(l) }
      e <- m.get(name)
    } yield {
      Text(e.code, e.text.format(params: _*))
    }) getOrElse Text("0", "???")
  }

  def loc(l: Language)(name: String, params: Seq[String]): Text = loc(null, l)(name, params)

  def loc0(l: Language)(name: String): Text = loc(null, l)(name, Nil)

}

case class LocEntry(code: String, name: String, text: String)
case class Text(code: String, text: String)

case class Language(language: String, country: Option[String] = None, variant: Option[String] = None) {
  override def toString = { 
    (country, variant) match {
      case (Some(c), Some(v)) => s"${language}_${c}_${v}"
      case (Some(c), None) => s"${language}_${c}"
      case (None, Some(v)) => s"${language}__${v}"
      case (None, None) => s"${language}"
    }
  }
}
