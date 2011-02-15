package net.shift
package http

import scala.xml._
import app._
import util._
import Util._
import Application._
import State._

private[http] object Server extends Reader {

  def boot(ctx: Context) = {
    Application.setup(ctx, new BootConfig(){})
  }

  def run = {

    for (req <- readRequest;
         page <- template(Resources.pageAsXml(req.path) getOrElse <Crap/>)
       ) yield {
      XhtmlResponse(page theSeq(0))
    }
  }
  
}


case class ServerState(req: Request, 
		       respStatus: Int, 
		       error: Option[Exception])
