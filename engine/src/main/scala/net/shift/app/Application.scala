package net.shift
package app

import http._

trait Boot {
  def boot: BootConfig
}

object Application extends BootConfig {

  type AccessControlFunc = Request => ((Request => Response) => Response)

  private var ctx: Context = _
  private var cfg: BootConfig = _

  private[shift] def setup(ctx: Context, cfg : BootConfig) {
    this.ctx = ctx
    this.cfg = cfg
  }

  def context = ctx
  
  override def rewrite = cfg.rewrite
  override def contextPath = cfg.contextPath
  override def handleRequest(req: Request) = cfg.handleRequest(req)
  override def templateLookupSuffixes = cfg.templateLookupSuffixes
  override def siteMap = cfg.siteMap


}

