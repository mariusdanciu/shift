package net.shift 
package http 

import java.io.{InputStream, OutputStream}
import java.net.URL
import net.shift.util._
import Util._

trait Request {
  def path: Path
  def method: String
  def contextPath: String
  def queryString: Option[String]
  def param(name: String): Option[String]
  def params(name: String): List[String]
  def params: Map[String, List[String]]
  def header(name: String): Option[String]
  def headers(name: String): List[String]
  def headers: Map[String, List[String]]
  def contentLength: Option[Long]
  def contentType: Option[String]
  def cookies: Map[String, Cookie]
  def cookie(name: String): Option[Cookie]
  def inputStream: InputStream 
}


trait Response {
  def code: Int
  def reason: Option[String]
  def headers: List[(String, String)]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def writeBody(out: OutputStream)
}

trait Context {
  def resourceAsStream(res: String): Option[InputStream]
}

object Path {
  def apply(str: String): Path = {
    val endSlash =  str.endsWith("/")
    val abs = str.startsWith("/")
    var uri = str split "/" toList

    uri = if (abs) uri tail else uri

    new Path(uri, endSlash, abs)
  }
 
  val empty = Path("") 
}

case class Path(parts: List[String], endSlash: Boolean, absolute: Boolean)

object Cookie {
  def apply(name: String, value: String) =
    new Cookie(name, Some(value), None, None, None, None, None)

  def apply(name: String, maxAge: Int) =
    new Cookie(name, None, None, None, Some(maxAge), None, None)
}

/**
 * Repersents an immutable representation of an HTTP Cookie
 */
case class Cookie(name: String,
                  value: Option[String],
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Int],
                  version: Option[Int],
                  secure_? : Option[Boolean])

object Request {
  val req = new Scope[Request]
  def unapply(req: Request): Option[(List[String], String)] = Some((req.path.parts, req.method))
  def apply(req: Request): Request = new ReqShell(req)
}

class ReqShell(val req: Request) extends Request {

  private val qs = (for ((k, v) <- params.toList;
                         item <- v) yield k + "=" + item) match {
    case Nil => None
    case l => Some(l.mkString("&"))
  }

  private val reqParams = req.params
  private val reqHeaders = req.headers

  def path = req.path
  def method = req.method
  def contextPath = applyPf(req)(Application.contextPath).getOrElse("/")
  def queryString = qs
    
  def param(name: String) = params.get(name).map(_ head)
  def params(name: String) = params.get(name).getOrElse(Nil)
  def params = reqParams

  def header(name: String) = headers.get(name).map(_ head)
  def headers(name: String) = headers.get(name).getOrElse(Nil)
  def headers = reqHeaders

  def contentLength = req.contentLength
  def contentType = req.contentType

  lazy val cookies = req.cookies
  def cookie(name: String) = cookies.get(name)

  def inputStream = req.inputStream

  def withParams(extra: (String, String)*): Request = new ReqShell(this) {
    override def params = super.params ++ extra.map(e => (e._1, List(e._2)))
  }

  def withHeaders(extra: (String, String)*): Request = new ReqShell(this) {
    override def headers = super.headers ++ extra.map(e => (e._1, List(e._2)))
  }

  def withPath(newPath: Path): Request = new ReqShell(this) {
    override def path = newPath
  }
  
  def withMethod(newMethod: String): Request = new ReqShell(this) {
    override def method = newMethod
  }


}


