package net.shift.engine.http

object HeaderKeyValue {
  def unapply(h: Header): Option[(String, String)] = {
    Some((h.key, h.valueWithNoParams))
  }
}

object MultipartBoundry {
  private def extractBoundry(multipart: String): Option[String] = {
    val idx = multipart.indexOf("boundary=")
    if (idx >= 0)
      Some(multipart.substring(idx + 9))
    else None
  }

  def unapply(h: Header): Option[String] = {
    (h.value.startsWith("multipart/form-data"), extractBoundry(h.value)) match {
      case (true, Some(value)) => Some(value)
      case _                   => None
    }
  }
}

object ContentDisposition {
  def unapply(h: Header): Option[(String, Map[String, String])] = {
    if (h.key == "Content-Disposition") {
      val splits = h.value.split(";")
      val v = splits.head
      val params = splits.tail.map { v =>
        v.split("=").toList match {
          case k :: v :: Nil => (k.trim, v.trim)
          case k :: Nil      => (k.trim, "")
          case _             => ("", "")
        }
      } toMap

      Some((v, params))
    } else {
      None
    }
  }
}