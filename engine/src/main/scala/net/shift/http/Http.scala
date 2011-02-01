package net.shift 
package http 

import java.io.{InputStream, OutputStream}
import java.net.URL
import net.shift.util._
import Util._

trait Request {
  def path: Path
  def method : String
  def contextPath : String
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

class ReqShell(req: Request) extends Request {self =>

  override val path = req.path
  override val method = req.method
  override val contextPath = applyPf(req)(Application.contextPath).getOrElse("/")
  override val queryString = req.queryString
    
  def param(name: String) = params.get(name).map(_ head)
  def params(name: String) = params.get(name).getOrElse(Nil)
  override val params = req.params

  def header(name: String) = headers.get(name).map(_ head)
  def headers(name: String) = headers.get(name).getOrElse(Nil)
  override val headers = req.headers

  override val contentLength = req.contentLength
  override val contentType = req.contentType

  lazy val cookies = req.cookies
  def cookie(name: String) = cookies.get(name)

  override val inputStream = req.inputStream

  def extraParams (extra: (String, String)*): Request = new ReqShell(this) {
    override val params = self.params ++ extra.map(e => (e._1, List(e._2)))
  }

  def estraHeaders (extra: (String, String)*): Request = new ReqShell(this) {
    override val headers = self.headers ++ extra.map(e => (e._1, List(e._2)))
  }

  def applyPath(newPath: Path): Request = new ReqShell(this) {
    override val path = newPath
  }
  
  def applyMethod(newMethod: String): Request = new ReqShell(this) {
    override val method = newMethod
  }


}


