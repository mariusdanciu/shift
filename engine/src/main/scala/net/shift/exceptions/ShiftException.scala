package net.shift {
package exceptions {


object ShiftException {
  def apply: ShiftException = new ShiftException(null, null)
  def apply(msg: String): ShiftException = new ShiftException(msg, null)
  def apply(cause: Throwable): ShiftException = new ShiftException(null, cause)
}

case class ShiftException(msg: String, causeEx: Throwable) extends Exception(msg, causeEx) {
  def message: Option[String] = if (msg == null) None else Some(msg)
  def cause: Option[Throwable] = if (causeEx == null) None else Some(causeEx)
}

}
}
