package net.shift
package engine
package http

import java.io.FileNotFoundException
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex
import common.Base64
import common.Config
import common.Path
import common.State
import common.State._
import net.shift.common.ShiftFailure
import net.shift.common.State
import net.shift.engine.ex2Fail
import net.shift.engine.http.Request.augmentRequest
import net.shift.io.BinProducer
import net.shift.io.FileSystem
import net.shift.io.IO.toArray
import net.shift.loc.Language
import net.shift.security.BasicCredentials
import net.shift.security.Credentials
import net.shift.security.HMac
import net.shift.security.SecurityFailure
import net.shift.security.User
import net.shift.security.Users
import net.shift.security.Permission
import net.shift.engine.utils.ShiftUtils
import scala.util.Try

object HttpPredicates {
  val Pattern = new Regex("""\w+:\w*:w*""")

  implicit def httpMethod2State(m: HttpMethod): State[Request, Request] = state {
    r => if (m is r.method) Success((r, r)) else ShiftFailure.toTry
  }

  def permissions(failMsg: => String, p: Permission*)(implicit login: Credentials => Try[User], conf: Config): State[Request, User] =
    for {
      u <- userRequired(failMsg) if (u.hasAllPermissions(p: _*))
    } yield {
      u
    }

  def userRequired(failMsg: => String)(implicit login: Credentials => Try[User], conf: Config): State[Request, User] = state {
    r =>
      {
        ShiftUtils.computeUser(r) match {
          case Success(u) => Success((r, u))
          case Failure(t) => Failure(t)
        }
      }
  }

  def user(implicit login: Credentials => Try[User], conf: Config): State[Request, Option[User]] = state {
    r => Success((r, ShiftUtils.computeUser(r).toOption))
  }

  def authenticate(failMsg: => String, code: Int = 401)(implicit login: Credentials => Try[User], conf: Config): State[Request, User] = state {
    r =>
      {
        ShiftUtils.computeUser(r) match {
          case Success(user) => Success((r, user))
          case _             => Failure(SecurityFailure(failMsg, code))
        }
      }
  }

  def ajax: State[Request, Request] = state {
    r =>
      r.header("X-Requested-With") match {
        case Some(Header(_, "XMLHttpRequest")) => Success((r, r))
        case _                                 => ShiftFailure.toTry
      }
  }

  def multipartForm(implicit conf: Config): State[Request, MultiPartBody] = state {
    r =>
      r.header("Content-Type") match {
        case Some(MultipartBoundry(boundry)) =>
          (toArray(r.readBody)) match {
            case Success(arr) => MultipartParser(boundry).parse(arr).map(e => (r, e))
            case _            => ShiftFailure.toTry
          }
        case _ => ShiftFailure.toTry
      }
  }

  def path(path: String): State[Request, Request] = state {
    r =>
      if (r.path == Path(path)) Success((r, r)) else ShiftFailure.toTry
  }

  def path: State[Request, Path] = state {
    r => Success((r, r.path))
  }

  def hasAllParams(params: List[String]): State[Request, List[String]] = state {
    r => if (params.filter(p => r.params.contains(p)).size != params.size) ShiftFailure.toTry else Success((r, params))
  }

  def containsAnyOfParams(params: List[String]): State[Request, List[String]] = state {
    r =>
      params.filter(p => r.params.contains(p)) match {
        case Nil => ShiftFailure.toTry
        case p   => Success((r, p))
      }
  }

  def param(name: String): State[Request, String] = state {
    r =>
      r.param(name) match {
        case Some(v :: _) => Success((r, v))
        case _            => ShiftFailure.toTry
      }
  }

  def paramValues(name: String): State[Request, List[String]] = state {
    r =>
      r.param(name) match {
        case Some(v) => Success((r, v))
        case _       => ShiftFailure.toTry
      }
  }

  def hasAllHeaders(headers: List[String]): State[Request, List[String]] = state {
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) ShiftFailure.toTry else Success((r, headers))
  }

  def containsAnyOfHeaders(headers: List[String]): State[Request, List[String]] = state {
    r =>
      headers.filter(p => r.headers.contains(p)) match {
        case Nil => ShiftFailure.toTry
        case p   => Success((r, p))
      }
  }

  def header(name: String): State[Request, Header] = state {
    r =>
      r.header(name) match {
        case Some(v) => Success((r, v))
        case _       => ShiftFailure.toTry
      }
  }

  def startsWith(path: Path): State[Request, Path] = state {
    r => if (r.path.startsWith(path)) Success((r, path)) else ShiftFailure.toTry
  }

  def tailPath: State[Request, Path] = state {
    r =>
      r.path match {
        case Path(_, Nil) => ShiftFailure.toTry
        case Path(_, h :: rest) => Success((new RequestShell(r) {
          override def path = r.path tail
          override def uri = s"$path?${r.queryString}"
        }, Path(rest)))
      }
  }

  def xmlContent: State[Request, String] = state {
    r =>
      r.contentType.filter(c => c.startsWith("application/xml") || c.startsWith("text/xml")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure.toTry
      }
  }

  def jsonContent: State[Request, String] = state {
    r =>
      r.contentType.filter(c => c.startsWith("application/json") || c.startsWith("text/json")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure.toTry
      }
  }

  def req: State[Request, Request] = init[Request]

  def req(r: Request => Request): State[Request, Request] = initf[Request](r)

  def withLanguage(l: Language): State[Request, Request] = initf[Request](_ withLanguage l)

  def language: State[Request, Language] = state {
    r => Success((r, r.language))
  }

  def fileOf(path: Path)(implicit fs: FileSystem): State[Request, BinProducer] = state {
    r =>
      {
        fs.exists(path).flatMap { b =>
          if (b) {
            (fs reader (path)).map((r, _))
          } else {
            Failure(new FileNotFoundException(path toString))
          }
        }
      }
  }

  def lastModified(path: Path)(implicit fs: FileSystem): State[Request, Long] = state {
    r =>
      {
        fs.exists(path).flatMap { b =>
          if (b) {
            (fs lastModified (path)).map((r, _))
          } else {
            Failure(new FileNotFoundException(path toString))
          }
        }
      }
  }

}


