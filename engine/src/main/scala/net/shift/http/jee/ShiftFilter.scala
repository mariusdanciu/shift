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
import util._
import Util._

class ShiftFilter extends Filter {
  private var continuation : State[Response, ServerState] = _

  def init(config: FilterConfig) {
    Server.boot(new ServletContext(config.getServletContext))
    continuation = Server run
  }

  def destroy {
  }

  def doFilter(req: SReq, res: SResp, chain: FilterChain) {
    val request = new ServletRequest(req.asInstanceOf[HttpServletRequest])
    continuation(ServerState(request, 200, None)) match {
      case (resp, _) => toServletResponse(resp, res.asInstanceOf[HttpServletResponse])
      //case _ => chain.doFilter(req, res)
    }
  }

  private def toServletResponse(resp: Response, sResp: HttpServletResponse) {
    resp.contentType.map( sResp.setContentType )
    resp writeBody sResp.getOutputStream
  }
}

