package net.shift {
package http{

import scala.xml._

object Application {

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

  def resourceAsXml(res: String): Option[Node] = None

}

}
}
