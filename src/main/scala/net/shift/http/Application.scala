package net.shift {
package http{

object Application {

  var rewrite: PartialFunction[Request, Request] = {
    case req => req
  }

}

}
}
