package net.shift
package engine

package object http {

  type AsyncResponse = Response => Unit
  type Rule = Option[AsyncResponse => Unit]

}
