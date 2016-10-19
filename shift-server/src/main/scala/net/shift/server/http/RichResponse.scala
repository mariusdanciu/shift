package net.shift.server.http

import java.nio.ByteBuffer

import net.shift.common.Base64
import net.shift.common.Config
import net.shift.common.Path
import net.shift.io.IO
import net.shift.loc.Language
import net.shift.security.HMac
import net.shift.security.User


case class RichRequest(r: Request) {

  def withPath(p: Path) = r.copy(uri = r.uri.copy(path = p.toString))
  def withLanguage(l: Language) = r.copy(headers = r.headers.filter(l => l.name != "Accept-Language") ++
    List(TextHeader("Accept-Language", l.toHttpString)))
}

case class RichResponse(r: Response) {

  def cache(expires: Int, etag: String) = r.copy(headers = r.headers ++ List(
    TextHeader("Cache-Control", s"max-age=$expires"),
    TextHeader("ETag", etag)))

  def withCookies(c: SetCookie*) = r.copy(headers = r.headers ++ c)

  def withSecurityCookies(user: User)(implicit conf: Config): Response = {
    val org = user.org.map(_.name) getOrElse ""
    val identity = s"${user.name}:$org:${user.permissions.map(_.name).mkString(",")}"
    val computedSecret = Base64.encode(HMac.encodeSHA256(identity, conf.string("auth.hmac.salt", "SHIFT-HMAC-SALT")))

    withCookies(
      SetCookie("identity", Base64.encodeString(identity), None, Some("/"), Some(conf.long("auth.ttl", 1800)), None, false, true),
      SetCookie("secret", computedSecret, None, Some("/"), Some(conf.long("auth.ttl", 1800)), None, false, true))
  }

  def dropSecurityCookies: Response = {
    withCookies(
      SetCookie("identity", "", None, Some("/"), Some(0), None, false, true),
      SetCookie("secret", "", None, Some("/"), Some(0), None, false, true))
  }

  private def withBody(c: String, mime: String) = {
    val arr = ByteBuffer.wrap(c.getBytes("UTF-8"))

    r.copy(headers = r.headers ++ List(
      Headers.contentType(mime),
      Headers.contentLength(arr.limit)),
      body = IO.bufferProducer(arr))
  }

  def withTextBody(b: String) = withBody(b, ContentType.TextPlain)
  def withJsonBody(b: String) = withBody(b, ContentType.TextJson)
  def withJavascripBody(b: String) = withBody(b, ContentType.TextJavascript)
  def withCssBody(b: String) = withBody(b, ContentType.TextCss)
  def withHtmlBody(b: String) = withBody(b, ContentType.TextHtml)

  def withMime(mime: String) = {
    r.copy(headers = r.headers.filter {
      case TextHeader("Content-Type", _) => false
      case _                             => true
    } ++ List(Headers.contentType(mime)))
  }

  def withHeaders(h: TextHeader*) = {
    r.copy(headers = r.headers ++ h)
  }

  def withCode(cd: Int) = r.copy(code = cd)

}