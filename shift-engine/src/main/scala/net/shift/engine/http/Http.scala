
package net.shift
package engine
package http

import scala.annotation.tailrec
import scala.util.Try
import scala.xml.Node
import common.Path
import net.shift.common.Base64
import net.shift.common.Config
import net.shift.io.Cont
import net.shift.io.Data
import net.shift.io.BinConsumer
import net.shift.io.BinProducer
import net.shift.loc.Language
import net.shift.security.BasicCredentials
import net.shift.security.Credentials
import net.shift.security.HMac
import net.shift.security.User
import net.shift.io.FileSystem
import net.shift.io.EOF

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
  def readBody: BinProducer
  def resource(path: Path)(implicit fs: FileSystem): Try[BinProducer]
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
  def readBody = in.readBody
  def resource(path: Path)(implicit fs: FileSystem) = in resource path
  def language: Language = in language
}

trait Response {
  def code: Int
  def reason: String
  def headers: List[Header]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def body: BinProducer
}

class ResponseShell(in: Response) extends Response {
  def code = in code
  def reason = in reason
  def headers = in headers
  def contentType = in contentType
  def cookies = in cookies
  def body = in.body
}

object Header {
  def apply(key: String, value: String) = new Header(key, value, Map.empty)
}

case class Header(key: String, value: String, params: Map[String, String]) {
  def stringValue =
    if (!params.isEmpty)
      value + ";" + params.mkString(",")
    else
      value
}

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

  def code(statusCode: Int) = new ResponseShell(r) {
    override val code = statusCode
  }

  def headers(prop: (Header)*) = new ResponseShell(r) {
    override val headers = r.headers ++ List(prop: _*)
  }

  def cookies(c: Cookie*) = new ResponseShell(r) {
    override val cookies = r.cookies ++ c
  }

  def securityCookies(user: User): Response = {
    val org = user.org.map(_.name) getOrElse ""
    val identity = s"${user.name}:$org:${user.permissions.map(_.name).mkString(",")}"
    val computedSecret = Base64.encode(HMac.encodeSHA256(identity, Config.string("auth.hmac.salt", "SHIFT-HMAC-SALT")))

    cookies(
      Cookie("identity", Base64.encodeString(identity), None, Some("/"), Some(Config.long("auth.ttl", 1800)), None, false, true),
      Cookie("secret", computedSecret, None, Some("/"), Some(Config.long("auth.ttl", 1800)), None, false, true))
  }

  def dropSecurityCookies: Response = {
    cookies(
      Cookie("identity", "", None, Some("/"), Some(0), None, false, true),
      Cookie("secret", "", None, Some("/"), Some(0), None, false, true))
  }

  def withBody(content: String): Response = new ResponseShell(r) {
    override def body = new BinProducer {

      def apply[O](in: BinConsumer[O]): BinConsumer[O] = in match {
        case Cont(f) => f(Data(content.getBytes("UTF-8"))) match {
          case Cont(f) => f(EOF)
          case r       => r
        }
        case r => r
      }
    }
  }

  def withBody(content: Array[Byte]): Response = new ResponseShell(r) {
    override def body = new BinProducer {

      def apply[O](in: BinConsumer[O]): BinConsumer[O] = in match {
        case Cont(f) => f(Data(content)) match {
          case Cont(f) => f(EOF)
          case r       => r
        }
        case r => r
      }
    }
  }

  def withBody(content: BinProducer): Response = new ResponseShell(r) {
    override def body = content
  }

  def asText: Response = new ResponseShell(r) {
    override def contentType = Some("text/plain; charset=\"UTF-8\"")
  }
  def asHtml5: Response = new ResponseShell(r) {
    override def contentType = Some("text/html; charset=\"UTF-8\"")
  }
  def asXml: Response = new ResponseShell(r) {
    override def contentType = Some("text/xml; charset=\"UTF-8\"")
  }
  def asJson: Response = new ResponseShell(r) {
    override def contentType = Some("application/json; charset=\"UTF-8\"")
  }
  def asJavaScript: Response = new ResponseShell(r) {
    override def contentType = Some("text/javascript; charset=\"UTF-8\"")
  }
  def asCSS: Response = new ResponseShell(r) {
    override def contentType = Some("text/css; charset=\"UTF-8\"")
  }
  def asBinary: Response = new ResponseShell(r) {
    override def contentType = Some("application/octet-stream")
  }
}

trait Context {
  def resourceChannel[O](res: String, it: BinConsumer[O]): O
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
