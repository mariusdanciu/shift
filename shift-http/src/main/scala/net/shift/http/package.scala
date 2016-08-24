package net.shift

package object http {

  type AsyncResponse = HTTPResponse => Unit
  type HTTPService = (HTTPRequest, AsyncResponse) => Unit

}