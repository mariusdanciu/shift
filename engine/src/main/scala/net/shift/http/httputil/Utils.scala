package net.shift
package http
package httputil

import util._

trait Reader {
  def read = new Generator[Request, Option] {
    def unit[B](b: B): Option[B] = {
      if (b != null) Some(b) else None
    }

    def filter(f: Request => Boolean): Generator[Request, Option] = {
      this
    }
  }
}
