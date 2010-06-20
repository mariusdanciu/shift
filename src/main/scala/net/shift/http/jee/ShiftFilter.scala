package net.shift {
package http {
package jee {

import javax.servlet.{Filter, FilterChain, FilterConfig, ServletRequest => SReq, ServletResponse => SResp}
import javax.servlet.http._

class SHiftFilter extends Filter {

  def init(config: FilterConfig) {
  }

  def destroy {
  }

  def doFilter(req: SReq, res: SResp, chain: FilterChain) {
  }

}

}
}
}
