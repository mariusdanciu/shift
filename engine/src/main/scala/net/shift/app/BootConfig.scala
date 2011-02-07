package net.shift
package app

import http._

trait BootConfig {

  def rewrite : PartialFunction[Request, Request] = {
    case req => req
  }

  def contextPath : PartialFunction[Request, String] = {
    case req => req.contextPath
  }

  def handleRequest : (Request) => Boolean = (req) => {
    req.path match {
      case Path("static" :: _, _) => false  
      case _ => true
    }
  }

  def templateLookupSuffixes: List[String] = "html" :: "htm" :: Nil

  def siteMap: Pages = Pages(Nil)


}
