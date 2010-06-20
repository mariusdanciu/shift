package net.shift {
package http {
package jee {

import java.io.InputStream
import javax.servlet.http.{Cookie => SCookie, HttpServletRequest}
import http._
import net.shift.util.Util._
import collection._

class ServletRequest(val req: HttpServletRequest) extends Request {

  def uri: String = req.getRequestURI
  def path: List[String] = List.fromString(uri, '/')
  def method: String = req.getMethod
  def contextPath: String = req.getContextPath
  def queryString: Option[String] = toOption(req.getQueryString)
  def param(name: String): Option[String] = toOption(req.getParameter(name))
  def params(name: String): List[String] = List.fromArray(req.getParameterValues(name))
  lazy val params: List[(String, String)] = 
    enumToList[String](req.getParameterNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
      map(name => (name, param(name).getOrElse("")))
  def header(name: String): Option[String] = toOption(req.getHeader(name))
  def headers(name: String): List[String] = 
    enumToList[String](req.getHeaders(name).asInstanceOf[_root_.java.util.Enumeration[String]])
  lazy val headers: List[(String, String)] = 
    enumToList[String](req.getHeaderNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
      map(name => (name, header(name).getOrElse("")))
  def contentLength: Option[Long] = toOption(req.getContentLength)
  def contentType: Option[String] = toOption(req.getContentType)
  lazy val cookies: List[Cookie] = req.getCookies.toList.map(c => Cookie(c.getName,
      toOption(c.getValue),
      toOption(c.getDomain),
      toOption(c.getPath),
      toOption(c.getMaxAge),
      toOption(c.getVersion),
      toOption(c.getSecure))  
  )
  def cookie(name: String): Option[Cookie] = cookies.find(_.name == name)
  def inputStream: InputStream = req.getInputStream


}


}
}
}
