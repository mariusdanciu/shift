package net.shift

package object http {

  type AsyncResponse = Response => Unit

  type HTTPService = Request => AsyncResponse => Unit

  implicit def enrichResp(r: Response): RichResponse = new RichResponse(r)
  implicit def enrichReq(r: Request): RichRequest = new RichRequest(r)
}