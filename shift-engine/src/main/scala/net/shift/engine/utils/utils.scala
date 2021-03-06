package net.shift.engine.utils

import net.shift.server.http._

class ServiceUtils(service: ResponseFunc => Unit) {

  def withResponse(f: Response => Response): ResponseFunc => Unit =
    (ar) => service(r => ar(f(r)))

}