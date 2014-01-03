package net.shift
package loc

import java.io.FileInputStream
import java.util.Locale

import scala.util.Try
import scala.util.control.Exception._

import org.json4s._
import org.json4s.native.JsonMethods._

import scalax.io.Resource

object Loc {

  private var cache: Map[Locale, Map[String, LocEntry]] = Map.empty
  
  var location = "localization/" 

  implicit val formats = DefaultFormats

  private def load(name: String, l: Locale) {
    
    val t1 = Try {
      Resource.fromInputStream(new FileInputStream(s"$location$name${l.getCountry()}_${l.getLanguage()}.json")).string
    }
    
    val t2 = Try {
      Resource.fromInputStream(new FileInputStream(s"$location$name${l.getLanguage()}.json")).string
    }

    (t1 orElse t2) map { s =>
      val entries = (parse(s).extract[List[LocEntry]]).map(e => (e.name, e)).toMap
      cache = cache + (l -> entries)
    }

  }

  def loc(prefix: String, l: Locale)(name: String, params: Seq[String]): Text = {
    (for {
      m <- cache.get(l) orElse { load(s"${prefix}", l); cache.get(l) }
      e <- m.get(name)
    } yield {
      Text(e.code, e.text.format(params:_*))
    }) getOrElse Text("0", "???")
  }

  def loc(l: Locale)(name: String, params: Seq[String]): Text = loc("", l)(name, params)
  
  def loc0(l: Locale)(name: String): Text = loc(l)(name, Nil)
  
  def loc0(prefix : String, l: Locale)(name: String): Text = loc(prefix, l)(name, Nil)
}

case class LocEntry(code: String, name: String, text: String)
case class Text(code: String, text: String)