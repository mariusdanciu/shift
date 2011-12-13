package net.shift
package engine

import http._

object Engine {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse) {
    for (handle <- app.routes.map(r => r(request)).find(!_.isEmpty); 
         f <- handle) {
      f(response)
    }
  }

}
