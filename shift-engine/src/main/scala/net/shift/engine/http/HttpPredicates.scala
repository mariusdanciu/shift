package net.shift
package engine
package http

import common._
import PathUtils._
import State._

object HttpPredicates {

  implicit def httpMethod2State(m: HttpMethod): State[Request, Request] = state {
    r => if (m is r.method) Some((r, r)) else None
  }

  def path(path: String): State[Request, List[String]] = state {
    r => if (r.path == pathToList(path)) Some((r, r.path)) else None
  }
  
  def path: State[Request, List[String]] = state {
    r => Some((r, r.path))
  }

  def hasAllParams(params: List[String]): State[Request, Request] = state {
    r => if (params.filter(p => r.params.contains(p)).size != params.size) None else Some((r, r))
  }

  def containsAnyOfParams(params: List[String]): State[Request, Request] = state {
    r => if (params.filter(p => r.params.contains(p)).isEmpty) None else Some((r, r))
  }

  def hasAllHeaders(headers: List[String]): State[Request, Request] = state {
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) None else Some((r, r))
  }

  def containsAnyOfHeaders(headers: List[String]): State[Request, Request] = state {
    r => if (headers.filter(p => r.headers.contains(p)).isEmpty) None else Some((r, r))
  }

  def startsWith(path: String): State[Request, Request] = state {
    r => if (r.path.startsWith(pathToList(path))) Some((r, r)) else None
  }

  def tailPath: State[Request, List[String]] = state {
    r =>
      r.path match {
        case Nil => None
        case h :: rest => Some((new RequestShell(r) {
          override def path = r.path tail
        }, rest))
      }
  }

  def xmlContent: State[Request, String] = state {
    r => r.contentType.filter(c => c == "application/xml" || c == "text/xml").map(c => (r, c))
  }

  def jsonContent: State[Request, String] = state {
    r => r.contentType.filter(c => c == "application/json" || c == "text/json").map(c => (r, c))
  }
  
}


