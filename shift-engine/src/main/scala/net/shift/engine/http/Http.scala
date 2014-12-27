
package net.shift
package engine
package http

import scala.xml.Node
import scalax.io._
import common.Path
import net.shift.loc.Language
import scala.util.Try
import net.shift.common.Base64
import net.shift.security.Credentials
import net.shift.security.BasicCredentials
import net.shift.security.User
import net.shift.security.HMac
import net.shift.common.Config

trait Request {
  def path: Path
  def uri: String
  def method: String
  def contextPath: Path
  def queryString: Option[String]
  def param(name: String): Option[List[String]]
  def params: Map[String, List[String]]
  def header(name: String): Option[Header]
  def headers: Map[String, Header]
  def contentLength: Option[Long]
  def contentType: Option[String]
  def cookies: Map[String, Cookie]
  def cookie(name: String): Option[Cookie]
  def readBody: Input
  def resource(path: Path): Try[Input]
  def language: Language
}

class RequestShell(in: Request) extends Request {
  def path = in path
  def uri = in uri
  def method = in method
  def contextPath = in contextPath
  def queryString = in queryString
  def param(name: String) = in param name
  def params = in params
  def header(name: String) = in header name
  def headers = in headers
  def contentLength = in contentLength
  def contentType = in contentType
  def cookies = in cookies
  def cookie(name: String) = in cookie name
  def readBody = in readBody
  def resource(path: Path) = in resource path
  def language: Language = in language
}

object Header {
  def apply(key: String, value: String) = new Header(key, value, Map.empty)
}

case class Header(key: String, value: String, params: Map[String, String])

object Request {
  implicit def augmentRequest(r: Request): RichRequest = RichRequest(r)
}

object Response {
  implicit def augmentResponse(r: Response): RichResponse = RichResponse(r)
}

case class RichRequest(r: Request) {
  def withLanguage(l: Language) = new RequestShell(r) {
    override val language = l
  }

  def withHeader(prop: (String, Header)) = new RequestShell(r) {
    override val headers = r.headers + prop
  }

  def withoutHeader(name: String) = new RequestShell(r) {
    override val headers = r.headers - name
  }

  def withParam(prop: (String, String)) = new RequestShell(r) {
    override val params = r.params + (prop._1 -> List(prop._2))
  }

  def withoutParam(name: String) = new RequestShell(r) {
    override val params = r.params - name
  }
}

case class RichResponse(r: Response) {

  def withHeaders(prop: (String, Header)*) = new ResponseShell(r) {
    override val headers = r.headers ++ Map(prop: _*)
  }

  def withCookies(c: Cookie*) = new ResponseShell(r) {
    override val cookies = r.cookies ++ c
  }

  def withSecurityCookies(user: User): Response = {
    val org = user.org.map(_.name) getOrElse ""
    val identity = s"${user.name}:$org:${user.permissions.map(_.name).mkString(",")}"
    val computedSecret = Base64.encode(HMac.encodeSHA256(identity, Config.string("auth.hmac.salt", "SHIFT-HMAC-SALT")))

    withCookies(
      Cookie("identity", Base64.encodeString(identity), None, Some("/"), Some(Config.long("auth.ttl", 3600000)), None, false, true),
      Cookie("secret", computedSecret, None, Some("/"), Some(Config.long("auth.ttl", 3600000)), None, false, true))
  }

  def withoutSecurityCookies: Response = {
    withCookies(
      Cookie("identity", "", None, Some("/"), Some(-1), None, false, true),
      Cookie("secret", "", None, Some("/"), Some(-1), None, false, true))
  }

}

trait Response {
  def code: Int
  def reason: String
  def headers: Map[String, Header]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def writeBody(channel: Output)
}

class ResponseShell(in: Response) extends Response {
  def code = in code
  def reason = in reason
  def headers = in headers
  def contentType = in contentType
  def cookies = in cookies
  def writeBody(channel: Output) = in writeBody channel

}

trait Context {
  def resourceChannel(res: String): Option[Output]
  def resourceAsXml(res: String): Option[Node]
  def contextPath: String
}

object Cookie {
  def apply(name: String, value: String) =
    new Cookie(name, value, None, None, None, None, false, false)

  def apply(name: String, value: String, maxAge: Int) =
    new Cookie(name, value, None, None, Some(maxAge), None, false, false)
}

object Base64Cookie {
  def unapply(c: Cookie): Option[String] = Some(new String(Base64.decode(c.value)))
}

case class Cookie(name: String,
                  value: String,
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Long],
                  version: Option[Int],
                  secure: Boolean,
                  httpOnly: Boolean)

sealed trait HttpMethod {
  def is(name: String): Boolean
}
case object GET extends HttpMethod {
  def is(name: String) = name == "GET"
}
case object POST extends HttpMethod {
  def is(name: String) = name == "POST"
}
case object PUT extends HttpMethod {
  def is(name: String) = name == "PUT"
}
case object DELETE extends HttpMethod {
  def is(name: String) = name == "DELETE"
}
case object HEAD extends HttpMethod {
  def is(name: String) = name == "HEAD"
}

object RuleException {
  def apply() = new RuleException("");
}

class RuleException(msg: String) extends RuntimeException(msg) with util.control.NoStackTrace

trait HttpUtils {

  def extractHeader(line: String) = {
    for {
      (h, t) <- kvSlice(line, "\\s*:\\s*")
      (v, p) <- listSlice(t, "\\s*;\\s*")
      map <- mapSlice(p, "\\s*=\\s*")
    } yield {
      Header(h, v, map)
    }

  }

  def extractHeaderValue(k: String, line: String) = {
    for {
      (v, p) <- listSlice(line, "\\s*;\\s*")
      map <- mapSlice(p, "\\s*=\\s*")
    } yield {
      Header(k, v, map)
    }

  }

  private def kvSlice(in: String, split: String) = {
    in.split(split).toList match {
      case k :: v :: Nil => Some((k, v))
      case l             => None
    }
  }

  private def listSlice(in: String, split: String) = in.split(split).toList match {
    case h :: t => Some((h, t))
    case _      => None
  }

  private def mapSlice(in: List[String], split: String) = Some(Map((for {
    item <- in
    p <- kvSlice(item, split)
  } yield {
    (p._1, unquote(p._2))
  }): _*))

  private def unquote(in: String) = if (in.head == '\"' && in.last == '\"') in.tail.dropRight(1) else in
}

object Authorization {
  def unapply(h: Header): Option[Credentials] =
    try {
      if (h.key.equals("Authorization") && h.value.startsWith("Basic ")) {
        Base64.decodeString(h.value.substring(6)).split(":").toList match {
          case user :: password :: Nil =>
            Some(BasicCredentials(user, password))
          case _ => None
        }
      } else None
    } catch {
      case e: Exception => e.printStackTrace(); throw e
    }
}
