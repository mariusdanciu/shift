package net.shift
package util


import scala.io.Source
import scala.xml.NodeSeq
import scala.xml.parsing._
import http.Path
import app.Application._


object Resources {

  def pageAsXml(path: Path): Option[NodeSeq] = {
    if (!path.endSlash) {
      templateLookupSuffixes.map( suffix =>
	resourceAsXml("/" + path.toString + "." + suffix)
      ).find(_.isEmpty == false) getOrElse None
     } else {
      None
    }
    
  }

  def resourceAsXml(res: String): Option[NodeSeq] = try {
    context.resourceAsStream(res).map(s => XhtmlParser(Source.fromInputStream(s)))
  } catch {
    case e => None
  }


}
