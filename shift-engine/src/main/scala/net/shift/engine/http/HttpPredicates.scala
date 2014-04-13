package net.shift
package engine
package http

import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import common._
import common.State._
import scalax.io.Input
import scalax.io.Resource
import java.io.FileNotFoundException
import net.shift.loc.Language

trait HttpPredicates {

  implicit def httpMethod2State(m: HttpMethod): State[Request, Request] = state {
    r => if (m is r.method) Success((r, r)) else ShiftFailure[Request]
  }

  def path(path: String): State[Request, Request] = state {
    r =>
      if (r.path == Path(path)) Success((r, r)) else ShiftFailure[Request]
  }

  def path: State[Request, Path] = state {
    r => Success((r, r.path))
  }

  def hasAllParams(params: List[String]): State[Request, List[String]] = state {
    r => if (params.filter(p => r.params.contains(p)).size != params.size) ShiftFailure[Request] else Success((r, params))
  }

  def containsAnyOfParams(params: List[String]): State[Request, List[String]] = state {
    r =>
      params.filter(p => r.params.contains(p)) match {
        case Nil => ShiftFailure[Request]
        case p => Success((r, p))
      }
  }

  def hasAllHeaders(headers: List[String]): State[Request, List[String]] = state {
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) ShiftFailure[Request] else Success((r, headers))
  }

  def containsAnyOfHeaders(headers: List[String]): State[Request, List[String]] = state {
    r =>
      headers.filter(p => r.headers.contains(p)) match {
        case Nil => ShiftFailure[Request]
        case p => Success((r, p))
      }
  }

  def startsWith(path: Path): State[Request, Path] = state {
    r => if (r.path.startsWith(path)) Success((r, path)) else ShiftFailure[Request]
  }

  def tailPath: State[Request, Path] = state {
    r =>
      r.path match {
        case Path(Nil) => ShiftFailure[Request]
        case Path(h :: rest) => Success((new RequestShell(r) {
          override def path = r.path tail
          override def uri = s"$path?${r.queryString}"
        }, Path(rest)))
      }
  }

  def xmlContent: State[Request, String] = state {
    r =>
      r.contentType.filter(c => c == "application/xml" || c == "text/xml").map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _ => ShiftFailure[Request]
      }
  }

  def jsonContent: State[Request, String] = state {
    r =>
      r.contentType.filter(c => c == "application/json" || c == "text/json").map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _ => ShiftFailure[Request]
      }
  }

  def req: State[Request, Request] = init[Request]

  def req(r: Request => Request): State[Request, Request] = initf[Request](r)

  def withLanguage(l : Language): State[Request, Request] = initf[Request](_ withLanguage l)

  def fileOf(path: Path): State[Request, Input] = state {
    r =>
      {
        val sp = path toString

        if (scalax.file.Path.fromString(sp).exists)
          Try(Resource.fromInputStream(new BufferedInputStream(new FileInputStream(sp)))) match {
            case Success(input) => Success((r, input))
            case Failure(f) => Failure(f)
          }
        else {
          Failure(new FileNotFoundException(sp))
        }
      }
  }

}


