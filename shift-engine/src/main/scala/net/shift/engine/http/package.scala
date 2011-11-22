package net.shift
package engine

package object http {

  type AsyncResponse = Response => Unit
  type Route = Request => Option[AsyncResponse => Unit]

}
