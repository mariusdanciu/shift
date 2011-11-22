package net.shift
package engine

import http._

trait ShiftApplication {

  def routes : List[Route]

}
