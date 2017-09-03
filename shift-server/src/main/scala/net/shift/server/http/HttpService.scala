package net.shift.server.http

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

}
