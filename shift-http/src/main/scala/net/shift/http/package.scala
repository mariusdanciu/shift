package net.shift

package object http {

  type AsyncResponse = HTTPResponse => Unit

  type HTTPService = HTTPRequest => AsyncResponse => Unit

  implicit def enrichResp(r: HTTPResponse): RichResponse = new RichResponse(r)
  implicit def enrichReq(r: HTTPRequest): RichRequest = new RichRequest(r)
}