package net.shift
package engine

import scala.util.Try
package object http {

  type AsyncResponse = Response => Unit
  type Service = AsyncResponse => Unit
  type Attempt = Try[Service]

  implicit def serviceServiceUtils(s: Service): ServiceUtils = new ServiceUtils(s)
}
