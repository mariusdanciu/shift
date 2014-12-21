package net.shift.engine.http

class ServiceUtils(s: Service) {

  def withResponse(f: Response => Response): Service =
    (ar: AsyncResponse) => s((r: Response) => ar(f(r)))
}