package net.shift {
package http{

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

}

}
}
