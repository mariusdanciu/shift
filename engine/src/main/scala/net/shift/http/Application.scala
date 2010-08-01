package net.shift
package http

import scala.io._
import scala.xml.{NodeSeq}
import scala.xml.parsing._

object Application {

  type AccessControlFunc = Request => ((Request => Response) => Response)

  private[http] var context: Context = _

  var rewrite : PartialFunction[Request, Request] = {
    case req => req
  }

  var contextPath : PartialFunction[Request, String] = {
    case req => req.contextPath
  }

  var handleRequest : (Request) => Boolean = (req) => {
    req.path match {
      case Path("static" :: _, _, _) => false  
      case _ => true
    }
  }

  def resourceAsXml(res: String): Option[NodeSeq] = try {
    context.resourceAsStream(res).map(s => XhtmlParser(Source.fromInputStream(s)))
  } catch {
    case e => None
  }


  var siteMap: () => Pages = () => Pages(Nil)
}

