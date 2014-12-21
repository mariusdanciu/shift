package net.shift
package engine
package http

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.FileNotFoundException
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex
import common._
import common.State._
import net.shift.common.ShiftFailure
import net.shift.loc.Language
import net.shift.security.HMac
import net.shift.security.Organization
import net.shift.security.Permission
import net.shift.security.User
import scalax.io.Input
import scalax.io.Resource
import net.shift.security.SecurityFailure
import net.shift.security.BasicCredentials
import net.shift.security.Credentials
import net.shift.security.Users

trait HttpPredicates extends TimeUtils {
  val Pattern = new Regex("""\w+:\w*:w*""")

  implicit def httpMethod2State(m: HttpMethod): State[Request, Request] = state {
    r => if (m is r.method) Success((r, r)) else ShiftFailure[Request]
  }

  def user(implicit login: Credentials => Option[User]): State[Request, Option[User]] = state {
    r =>
      {
        val res = (r.header("Authorization"), r.cookie("identity"), r.cookie("secret")) match {
          case (_, Some(Cookie(_, Base64(identity), _, _, _, _, _, _)), Some(Cookie(_, secret, _, _, _, _, _, _))) =>
            val computedSecret = Base64.encode(HMac.encodeSHA256(identity, Config.string("hmac.auth.salt", "SHIFT-HMAC-SALT")))
            if (computedSecret == secret) {
              identity match {
                case Users(u) => Some(u)
                case _        => None
              }
            } else {
              None
            }
          case (Some(Authorization(creds @ BasicCredentials(user, password))), None, None) => login(creds)
          case _ => None
        }
        Success((r, res))
      }
  }

  def authenticate(implicit login: Credentials => Option[User]): State[Request, User] = state {
    r =>
      {
        (r.header("Authorization"), r.cookie("identity"), r.cookie("secret")) match {

          case (_, Some(Cookie(_, Base64(identity), _, _, _, _, _, _)), Some(Cookie(_, secret, _, _, _, _, _, _))) =>
            val computedSecret = Base64.encode(HMac.encodeSHA256(identity, Config.string("hmac.auth.salt", "SHIFT-HMAC-SALT")))
            if (computedSecret == secret) {
              identity match {
                case Users(u) => Success((r, u))
                case _        => Failure(SecurityFailure[User]("Incorrect identity"))
              }
            } else {
              Failure(SecurityFailure[User]("Secret does not match"))
            }

          case (Some(Authorization(creds @ BasicCredentials(user, password))), None, None) =>
            login(creds) match {
              case Some(u) =>
                Success((r, u))
              case _ =>
                Failure(SecurityFailure[User]("Login failed"))
            }

          case _ => Failure(SecurityFailure[User]("Cannot authenticate"))
        }
      }
  }

  def ajax: State[Request, Request] = state {
    r =>
      r.header("X-Requested-With") match {
        case Some(Header(_, "XMLHttpRequest", _)) => Success((r, r))
        case _                                    => ShiftFailure[Request]
      }
  }

  def multipartForm: State[Request, MultiPartBody] = state {
    r =>
      r.header("Content-Type") match {
        case Some(Header(_, "multipart/form-data", params)) =>
          params.get("boundary") match {
            case Some(b) =>
              val arr = (r readBody).byteArray
              MultipartParser(b).parse(arr).map(e => (r, e))
            case None => ShiftFailure[Request]
          }
        case _ => ShiftFailure[Request]
      }
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
        case p   => Success((r, p))
      }
  }

  def param(name: String): State[Request, String] = state {
    r =>
      r.param(name) match {
        case Some(v :: _) => Success((r, v))
        case _            => ShiftFailure[Request]
      }
  }

  def paramValues(name: String): State[Request, List[String]] = state {
    r =>
      r.param(name) match {
        case Some(v) => Success((r, v))
        case _       => ShiftFailure[Request]
      }
  }

  def hasAllHeaders(headers: List[String]): State[Request, List[String]] = state {
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) ShiftFailure[Request] else Success((r, headers))
  }

  def containsAnyOfHeaders(headers: List[String]): State[Request, List[String]] = state {
    r =>
      headers.filter(p => r.headers.contains(p)) match {
        case Nil => ShiftFailure[Request]
        case p   => Success((r, p))
      }
  }

  def header(name: String): State[Request, Header] = state {
    r =>
      r.header(name) match {
        case Some(v) => Success((r, v))
        case _       => ShiftFailure[Request]
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
      r.contentType.filter(c => c.startsWith("application/xml") || c.startsWith("text/xml")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure[Request]
      }
  }

  def jsonContent: State[Request, String] = state {
    r =>
      r.contentType.filter(c => c.startsWith("application/json") || c.startsWith("text/json")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure[Request]
      }
  }

  def req: State[Request, Request] = init[Request]

  def req(r: Request => Request): State[Request, Request] = initf[Request](r)

  def withLanguage(l: Language): State[Request, Request] = initf[Request](_ withLanguage l)

  def language: State[Request, Language] = state {
    r => Success((r, r.language))
  }

  def fileOf(path: Path): State[Request, Input] = state {
    r =>
      {
        val sp = path toString

        if (scalax.file.Path.fromString(sp).exists)
          Try(Resource.fromInputStream(new BufferedInputStream(new FileInputStream(sp)))) match {
            case Success(input) => Success((r, input))
            case Failure(f)     => Failure(f)
          }
        else {
          Failure(new FileNotFoundException(sp))
        }
      }
  }

}


