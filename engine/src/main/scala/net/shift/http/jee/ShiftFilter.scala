package net.shift
package http
package jee

import javax.servlet.{Filter, 
  FilterChain, 
  FilterConfig, 
  ServletRequest => SReq, 
  ServletResponse => SResp,
  ServletContext => SCtx
}

import javax.servlet.http._
import net.shift.util.Util._
import Application._

class ShiftFilter extends Filter {

  def init(config: FilterConfig) {
    Server.boot(new ServletContext(config.getServletContext))
    Server run
  }

  def destroy {
  }

  def doFilter(req: SReq, res: SResp, chain: FilterChain) {
    val request = new ServletRequest(req.asInstanceOf[HttpServletRequest])
    Server.continuation(request) match {
      case Some(resp) => toServletResponse(resp, res.asInstanceOf[HttpServletResponse])
      case _ => chain.doFilter(req, res)
    }
  }

  private def toServletResponse(resp: Response, sResp: HttpServletResponse) {
    resp.contentType.map( sResp.setContentType )
    resp writeBody sResp.getOutputStream
  }
}

