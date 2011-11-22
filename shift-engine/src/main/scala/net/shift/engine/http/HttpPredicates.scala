package net.shift
package engine
package http

import common._

object HttpPredicates {

  type Kleisli[A, B, M[+_]] = A => M[B]

  implicit def f2Kleisli[A, B](f: A => Option[B]): OptionCombinatorsView[A, B] = 
    new OptionCombinatorsView(f)

  implicit def method2Kleisli(m : HttpMethod) : Kleisli[Request, Request, Option] = 
     r => if (r.method == m) Some(r) else None

  def fullPath(path: String): Kleisli[Request, Request, Option] = 
    r => if (r.path == path) Some(r) else None

  def hasParams(path: List[String]): Kleisli[Request, Request, Option] = 
    r => if (path.filter(p => r.params.contains(p)).size != path.size) None else
      Some(r)

  def hasAnyParam(path: List[String]): Kleisli[Request, Request, Option] = 
    r => if (path.filter(p => r.params.contains(p)).isEmpty) None else
      Some(r)

  def hasHeaders(path: List[String]): Kleisli[Request, Request, Option] = 
    r => if (path.filter(p => r.headers.contains(p)).size != path.size) None else
      Some(r)

  def hasAnyHeader(path: List[String]): Kleisli[Request, Request, Option] = 
    r => if (path.filter(p => r.headers.contains(p)).isEmpty) None else
      Some(r)

  def startsWith(path: String): Kleisli[Request, Request, Option] = 
    r => if (r.path == path) Some(r) else None

  def xmlContent: Kleisli[Request, Request, Option] = 
    r => r.contentType.filter(c => c == "application/xml" || c == "text/xml").map(c => r)

  def jsonContent: Kleisli[Request, Request, Option] = 
    r => r.contentType.filter(c => c == "application/json" || c == "text/json").map(c => r)

  def anyOf(f: (Request => Option[Request])*): Kleisli[Request, Request, Option] = null

}
