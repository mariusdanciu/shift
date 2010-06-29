package net.shift {
package http {
package jee {

import java.io.InputStream
import javax.servlet.http.{Cookie => SCookie, HttpServletRequest}
import http._
import net.shift.util.Util._
import collection.immutable._

class ServletRequest(val req: HttpServletRequest) extends Request {

  lazy val path: List[String] = {
    val uri = List.fromString(req.getRequestURI, '/')
    if (contextPath != "/") uri.drop(1)
      else uri
  }
  lazy val method: String = req.getMethod
  lazy val contextPath: String = req.getContextPath
  lazy val queryString: Option[String] = toOption(req.getQueryString)

  def param(name: String): Option[String] = params.get(name).map(_.head)
  def params(name: String): List[String] = params.get(name).getOrElse(Nil)
  lazy val params: Map[String, List[String]] = 
    Map.empty ++ enumToList[String](req.getParameterNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
      map(name => (name, enumToList[String](req.getParameterValues(name).
        asInstanceOf[_root_.java.util.Enumeration[String]])))

 
  def header(name: String): Option[String] = toOption(req.getHeader(name))
  def headers(name: String): List[String] = headers.get(name).getOrElse(Nil)
  lazy val headers: Map[String, List[String]] = 
    Map.empty ++ enumToList[String](req.getHeaderNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
      map(name => (name, enumToList[String](req.getHeaders(name).
        asInstanceOf[_root_.java.util.Enumeration[String]])))

  lazy val contentLength: Option[Long] = toOption(req.getContentLength)
  lazy val contentType: Option[String] = toOption(req.getContentType)
  lazy val cookies: Map[String, Cookie] = Map.empty ++ req.getCookies.toList.map(c => (c.getName, Cookie(c.getName,
      toOption(c.getValue),
      toOption(c.getDomain),
      toOption(c.getPath),
      toOption(c.getMaxAge),
      toOption(c.getVersion),
      toOption(c.getSecure))))  

  def cookie(name: String): Option[Cookie] = cookies.get(name)
  def inputStream: InputStream = req.getInputStream


}


}
}
}
