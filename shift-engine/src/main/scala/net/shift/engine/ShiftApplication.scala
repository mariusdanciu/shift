package net.shift
package engine

import http._

trait ShiftApplication {
  def rules : List[Rule]
}
