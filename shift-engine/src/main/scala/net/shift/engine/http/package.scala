package net.shift
package engine

package object http {

  type AsyncResponse = Response => Unit
  type Rule = Request => Option[AsyncResponse => Unit]

}
