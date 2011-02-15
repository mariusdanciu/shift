package net.shift
package app

import scala.xml._
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

  def pageNotFound (req : Request) : NodeSeq =
 <html>
  <body>
   <h1>{"Page " + req.path + " was not found."}</h1>
   <h2>Replace this page with a proper one</h2>
  </body> 
 </html>

}
