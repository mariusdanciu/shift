package net.shift
package engine

import http._

object Engine {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse) {
    for (f <- app.servingRule(request)) {
      f(response)
    }
  }
}
