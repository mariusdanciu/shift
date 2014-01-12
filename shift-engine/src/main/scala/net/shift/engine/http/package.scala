package net.shift
package engine

import scala.util.Try
package object http {

  type AsyncResponse = Response => Unit
  type Rule = Try[AsyncResponse => Unit]

}
