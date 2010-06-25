package net.shift {
package http {

import java.io.{InputStream, OutputStream}
import net.shift.util.Util._

trait Request {

  def path: List[String]
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
  def headers: List[String]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def drain(out: OutputStream)
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
  def unapply(req: Request): Option[(List[String], String)] = Some((req.path, req.method))

  def apply(req: Request): Request = new ReqShell(req)

}

class ReqShell(val req: Request) extends Request {
  def path = req.path
  def method = req.method
  def contextPath = applyPf(req)(Application.contextPath).getOrElse("/")
  def queryString = 
    (for ((k, v) <- params.toList;
           item <- v) yield (k, item)) match {
    case Nil => None
    case l => Some(l.mkString("&"))
  }
  def param(name: String) = params.get(name).map(_ head)
  def params(name: String) = params.get(name).getOrElse(Nil)
  lazy val params = req.params ++ paramsFunc()
  def header(name: String) = headers.get(name).map(_ head)
  def headers(name: String) = headers.get(name).getOrElse(Nil)
  lazy val headers = req.headers
  def contentLength = req.contentLength
  def contentType = req.contentType
  lazy val cookies = req.cookies
  def cookie(name: String) = cookies.get(name)
  def inputStream = req.inputStream

  private var paramsFunc: () => Map[String, List[String]] = () => Map.empty
  private var headersFunc: () => Map[String, List[String]] = () => Map.empty
  private var pathFunc: () => List[String] = () => Nil

  def withParams(extraParams: Map[String, List[String]]): Request = new ReqShell(this) {
    paramsFunc = () => extraParams
  }
}

}
}
