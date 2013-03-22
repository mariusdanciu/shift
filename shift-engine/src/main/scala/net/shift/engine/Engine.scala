package net.shift
package engine

import http._

object Engine {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse) {

    app.servingRule.map {
      case f => f(request) match {
        case Some(g) => g(response)
        case _ => Console.println("Not found " + request.path)
      }
    }(request)

  }

}
