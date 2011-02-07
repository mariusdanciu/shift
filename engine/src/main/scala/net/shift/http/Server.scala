package net.shift
package http

import app._
import util._
import http.httputil._
import Util._
import Application._


private[http] object Server extends Reader {

  def boot(ctx: Context) = {
    Application.setup(ctx, new BootConfig(){})
  }

  def run = {

    for (r @ Request(path, method) <- read;
         req <- applyPf(r)(rewrite);
         template <- Resources.pageAsXml(req.path)
        ) yield {
      XhtmlResponse(template theSeq(0))
    }
  }
  
}


