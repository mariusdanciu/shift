package net.shift.http

import net.shift.security.HMac
import net.shift.common.Config
import net.shift.security.User
import net.shift.common.Base64

case class RichResponse(r: HTTPResponse) {

  def cache(expires: Int, etag: String) = r.copy(headers = r.headers ++ List(
    TextHeader("Cache-Control", s"max-age=$expires"),
    TextHeader("ETag", etag)))

  def withCookies(c: SetCookie*) = r.copy(headers = r.headers ++ c)

  def securityCookies(user: User)(implicit conf: Config): HTTPResponse = {
    val org = user.org.map(_.name) getOrElse ""
    val identity = s"${user.name}:$org:${user.permissions.map(_.name).mkString(",")}"
    val computedSecret = Base64.encode(HMac.encodeSHA256(identity, conf.string("auth.hmac.salt", "SHIFT-HMAC-SALT")))

    withCookies(
      SetCookie("identity", Base64.encodeString(identity), None, Some("/"), Some(conf.long("auth.ttl", 1800)), None, false, true),
      SetCookie("secret", computedSecret, None, Some("/"), Some(conf.long("auth.ttl", 1800)), None, false, true))
  }

  def dropSecurityCookies: HTTPResponse = {
    withCookies(
      SetCookie("identity", "", None, Some("/"), Some(0), None, false, true),
      SetCookie("secret", "", None, Some("/"), Some(0), None, false, true))
  }

}