package net.shift
package common

object FileSplit {

  def unapply(s: Path): Option[(String, String)] = unapply(s.toString())

  def unapply(s: String): Option[(String, String)] = {
    val pos = s.lastIndexOf(".")
    if (pos > -1) {
      val t = s.splitAt(pos + 1)
      Some((t._1.dropRight(1), t._2))
    } else None
  }

}
