package net.shift
package engine

import http._

object Engine {

  def run(app: ShiftApplication)(request: Request, response: AsyncResponse) {

    app.servingRule.map {
      f => f map { _(response) }
    } (request)

  }

}
