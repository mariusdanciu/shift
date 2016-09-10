package net.shift.engine.http

import net.shift.http.TextHeader
import net.shift.security.BasicCredentials
import net.shift.security.Credentials
import net.shift.common.Base64

object Authorization {

  def unapply(h: TextHeader): Option[Credentials] =
    try {
      if (h.name.equals("Authorization") && h.value.startsWith("Basic ")) {
        Base64.decodeString(h.value.substring(6)).split(":").toList match {
          case Nil =>
            Some(BasicCredentials("", ""))
          case u :: Nil =>
            Some(BasicCredentials(u, ""))
          case user :: password :: Nil =>
            Some(BasicCredentials(user, password))
          case _ => None
        }
      } else None
    } catch {
      case e: Exception => None
    }

}