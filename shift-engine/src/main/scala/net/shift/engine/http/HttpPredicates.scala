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

  def path(path: String): Kleisli[Request, Request, Option] = 
    r => if (r.path == (path split "/").toList) Some(r) else None
  
  def hasParams(params: List[String]): Kleisli[Request, Request, Option] = 
    r => if (params.filter(p => r.params.contains(p)).size != params.size) None else Some(r)

  def hasAnyParam(params: List[String]): Kleisli[Request, Request, Option] = 
   r => if (params.filter(p => r.params.contains(p)).isEmpty) None else Some(r)

  def hasHeaders(headers: List[String]): Kleisli[Request, Request, Option] = 
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) None else Some(r)

  def hasAnyHeader(headers: List[String]): Kleisli[Request, Request, Option] = 
    r => if (headers.filter(p => r.headers.contains(p)).isEmpty) None else Some(r)

  def startsWith(path: String): Kleisli[Request, Request, Option] = 
    r => if (r.path.startsWith((path split "/").toList)) Some(r) else None
    
  def tailPath : Kleisli[Request, Request, Option] = 
    r => Some(new RequestShell(r) {
      override def path = r.path match {
        case Nil => Nil
        case p => p.tail
      }
    })
   
   
  def xmlContent: Kleisli[Request, Request, Option] = 
    r => r.contentType.filter(c => c == "application/xml" || c == "text/xml").map(c => r)

  def jsonContent: Kleisli[Request, Request, Option] = 
    r => r.contentType.filter(c => c == "application/json" || c == "text/json").map(c => r)

}


