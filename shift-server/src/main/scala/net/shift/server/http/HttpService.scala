package net.shift.server.http

import net.shift.common.{Log, LogBuilder}

/**
  * Created by marius on 9/3/2017.
  */

object HttpService {
  def build(f: Request => ResponseFunc => Unit): HttpService = new HttpService {
    override def apply(req: Request): (ResponseFunc) => Unit = resp => {
      f(req)(resp)
    }
  }
}

trait HttpService extends (Request => ResponseFunc => Unit) {
  protected val log: Log = LogBuilder.logger(classOf[HttpService])

  def onError(t: Throwable): Unit = {
    log.error("Server failure:", t)
  }

  def onTerminate(): Unit = {
    log.info("Server shutdown")
  }
}
