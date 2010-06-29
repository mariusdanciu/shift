package net.shift {
package http{

import scala.io._
import scala.xml._
import parsing._

object Application {

  private[http] var context: Context = _

  var rewrite : PartialFunction[Request, Request] = {
    case req => req
  }

  var contextPath : PartialFunction[Request, String] = {
    case req => req.contextPath
  }

  var handle_? : (Request) => Boolean = (req) => {
    req.path match {
      case "static" :: _ => false  
      case _ => true
    }
  }

  def resourceAsXml(res: String): Option[NodeSeq] = try {
    context.resourceAsStream(res).map(s => XhtmlParser(Source.fromInputStream(s)))
  } catch {
    case e => None
  }

}

}
}
