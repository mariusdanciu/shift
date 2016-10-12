package net.shift.engine.utils

import net.shift.http._

class ServiceUtils(service: AsyncResponse => Unit) {

  def withResponse(f: Response => Response): AsyncResponse => Unit =
    (ar) => service(r => ar(f(r)))

}