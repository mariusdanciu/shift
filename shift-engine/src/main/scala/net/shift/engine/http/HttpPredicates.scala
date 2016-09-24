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
import net.shift.io.BinProducer
import net.shift.io.FileSystem
import net.shift.loc.Language
import net.shift.security.BasicCredentials
import net.shift.security.Credentials
import net.shift.security.HMac
import net.shift.security.SecurityFailure
import net.shift.security.User
import net.shift.security.Users
import net.shift.security.Permission
import scala.util.Try
import net.shift.http.HTTPRequest
import net.shift.http.TextHeader
import net.shift.http.Responses
import net.shift.http.HTTPResponse
import net.shift.common.FileSplit
import net.shift.http.ExtentionToMime
import net.shift.http.Cookie
import net.shift.http.ContentType
import engine.ShiftApplication._

object HttpPredicates {
  val Pattern = new Regex("""\w+:\w*:w*""")

  def get: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "GET") Success((r, r)) else ShiftFailure.toTry
  }

  def post: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "POST") Success((r, r)) else ShiftFailure.toTry
  }

  def put: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "PUT") Success((r, r)) else ShiftFailure.toTry
  }

  def delete: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "DELETE") Success((r, r)) else ShiftFailure.toTry
  }

  def options: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "OPTIONS") Success((r, r)) else ShiftFailure.toTry
  }

  def trace: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "TRACE") Success((r, r)) else ShiftFailure.toTry
  }

  def connect: State[HTTPRequest, HTTPRequest] = state {
    r => if (r.method == "CONNECT") Success((r, r)) else ShiftFailure.toTry
  }

  def permissions(failMsg: => String, p: Permission*)(implicit login: Credentials => Try[User], conf: Config): State[HTTPRequest, User] =
    for {
      u <- userRequired(failMsg) if (u.hasAllPermissions(p: _*))
    } yield {
      u
    }

  def userRequired(failMsg: => String)(implicit login: Credentials => Try[User], conf: Config): State[HTTPRequest, User] = state {
    r =>
      {
        computeUser(r) match {
          case Success(u) => Success((r, u))
          case Failure(t) => Failure(t)
        }
      }
  }

  def user(implicit login: Credentials => Try[User], conf: Config): State[HTTPRequest, Option[User]] = state {
    r => Success((r, computeUser(r).toOption))
  }

  def authenticate(failMsg: => String, code: Int = 401)(implicit login: Credentials => Try[User], conf: Config): State[HTTPRequest, User] = state {
    r =>
      {
        computeUser(r) match {
          case Success(user) => Success((r, user))
          case _             => Failure(SecurityFailure(failMsg, code))
        }
      }
  }

  def ajax: State[HTTPRequest, HTTPRequest] = state {
    r =>
      r.header("X-Requested-With") match {
        case Some(TextHeader(_, "XMLHttpRequest")) => Success((r, r))
        case _                                     => ShiftFailure.toTry
      }
  }

  def multipartForm(implicit conf: Config): State[HTTPRequest, MultiPartBody] = state {
    r =>
      r.header("Content-Type") match {
        case Some(MultipartBoundry(boundry)) => MultipartParser(boundry).parse(r.body).map(e => (r, e))
        case _                               => ShiftFailure.toTry
      }
  }

  def path(path: String): State[HTTPRequest, HTTPRequest] = state {
    r =>
      if (r.uri.path == path)
        Success((r, r))
      else
        ShiftFailure.toTry
  }

  def exceptPath(path: String): State[HTTPRequest, HTTPRequest] = state {
    r =>
      if (r.uri.path != path)
        Success((r, r))
      else
        ShiftFailure.toTry
  }

  def path: State[HTTPRequest, Path] = state {
    r =>
      Success((r, Path(r.uri.path)))
  }

  def hasAllParams(params: List[String]): State[HTTPRequest, List[String]] = state {
    r => if (params.filter(p => r.uri.params.contains(p)).size != params.size) ShiftFailure.toTry else Success((r, params))
  }

  def containsAnyOfParams(params: List[String]): State[HTTPRequest, List[String]] = state {
    r =>
      params.filter(p => r.uri.params.contains(p)) match {
        case Nil => ShiftFailure.toTry
        case p   => Success((r, p))
      }
  }

  def param(name: String): State[HTTPRequest, String] = state {
    r =>
      r.uri.paramValue(name) match {
        case Some(v :: _) => Success((r, v))
        case _            => ShiftFailure.toTry
      }
  }

  def paramValues(name: String): State[HTTPRequest, List[String]] = state {
    r =>
      r.uri.paramValue(name) match {
        case Some(v) => Success((r, v))
        case _       => ShiftFailure.toTry
      }
  }

  def hasAllHeaders(headers: List[String]): State[HTTPRequest, List[String]] = state {
    r => if (headers.filter(p => r.headers.contains(p)).size != headers.size) ShiftFailure.toTry else Success((r, headers))
  }

  def containsAnyOfHeaders(headers: List[String]): State[HTTPRequest, List[String]] = state {
    r =>
      headers.filter(p => r.headers.contains(p)) match {
        case Nil => ShiftFailure.toTry
        case p   => Success((r, p))
      }
  }

  def headerValue(name: String): State[HTTPRequest, String] = state {
    r =>
      r.header(name) match {
        case Some(v: TextHeader) => Success((r, v.value))
        case _                   => ShiftFailure.toTry
      }
  }

  def startsWith(path: Path): State[HTTPRequest, Path] = state {
    r => if (Path(r.uri.path).startsWith(path)) Success((r, path)) else ShiftFailure.toTry
  }

  def tailPath: State[HTTPRequest, Path] = state {
    r =>
      Path(r.uri.path) match {
        case Path(_, Nil) => ShiftFailure.toTry
        case Path(_, h :: rest) =>
          val p = Path(rest)
          Success((r.withPath(p), p))
      }
  }

  def xmlContent: State[HTTPRequest, String] = state {
    r =>
      r.stringHeader("Content-Type").filter(c => c.startsWith("application/xml") || c.startsWith("text/xml")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure.toTry
      }
  }

  def jsonContent: State[HTTPRequest, String] = state {
    r =>
      r.stringHeader("Content-Type").filter(c => c.startsWith("application/json") || c.startsWith("text/json")).map(c => (r, c)) match {
        case Some(s) => Success(s)
        case _       => ShiftFailure.toTry
      }
  }

  def req: State[HTTPRequest, HTTPRequest] = init[HTTPRequest]

  def req(r: HTTPRequest => HTTPRequest): State[HTTPRequest, HTTPRequest] = initf[HTTPRequest](r)

  def withLanguage(l: Language): State[HTTPRequest, HTTPRequest] = initf[HTTPRequest](_ withLanguage l)

  def language: State[HTTPRequest, Language] = state {
    r => Success((r, r.language))
  }

  def fileOf(path: Path)(implicit fs: FileSystem): State[HTTPRequest, BinProducer] = state {
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

  def fileAsResponse(path: Path, mime: String)(implicit fs: FileSystem): State[HTTPRequest, HTTPResponse] = state {
    r => Responses.fileResponse(path, mime) map { (r, _) }
  }

  def lastModified(path: Path)(implicit fs: FileSystem): State[HTTPRequest, Long] = state {
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

  def fileSize(path: Path)(implicit fs: FileSystem): State[HTTPRequest, Long] = state {
    r =>
      {
        fs.exists(path).flatMap { b =>
          if (b) {
            (fs fileSize (path)).map((r, _))
          } else {
            Failure(new FileNotFoundException(path toString))
          }
        }
      }
  }

  def staticFiles(folder: Path)(implicit fs: FileSystem) = for {
    r <- req
    Path(_, _ :: "static" :: p) <- path
    val localPath = folder + p.mkString("/")
    input <- fileOf(localPath)
    lastMod <- lastModified(localPath)
    size <- fileSize(localPath)
  } yield {

    service { resp =>
      r.header("If-None-Match") match {
        case Some(TextHeader(_, etag)) if (etag == lastMod.toString) =>
          resp(Responses.notModified)
        case _ =>
          val FileSplit(name, ext) = p.last

          val mime = ExtentionToMime.map.get(ext) getOrElse ContentType.Bin

          val r = Responses.producerResponse(input, size).withMime(mime)

          resp(r.cache(240, lastMod.toString))
      }
    }
  }

  def computeUser(r: HTTPRequest)(implicit login: Credentials => Try[User], conf: Config): Try[User] = {
    (r.header("Authorization"), r.cookie("identity"), r.cookie("secret")) match {
      case (Some(Authorization(creds @ BasicCredentials(user, password))), None, None) =>
        login(creds)

      case (_, Some(Cookie(_, Base64(identity))), Some(Cookie(_, secret))) =>
        val computedSecret = Base64.encode(HMac.encodeSHA256(identity, conf.string("hmac.auth.salt", "SHIFT-HMAC-SALT")))
        if (computedSecret == secret) {
          identity match {
            case Users(u) => Success(u)
            case _        => ShiftFailure("Identity is incorrect.").toTry
          }
        } else {
          ShiftFailure("Identity is incorrect.").toTry
        }

      case _ => ShiftFailure("Cannot determine the user.").toTry
    }
  }

  def staticFile(file: Path)(implicit fs: FileSystem) = for {
    r <- req if (r.uri.path == file)
    val localPath = Path(r.uri.path)
    input <- fileOf(localPath)
    lastMod <- lastModified(localPath)
    size <- fileSize(localPath)
  } yield {

    service { resp =>
      r.header("If-None-Match") match {
        case Some(TextHeader(_, etag)) if (etag == lastMod.toString) =>
          resp(Responses.notModified)
        case _ =>
          val FileSplit(name, ext) = localPath.parts.last

          val mime = ExtentionToMime.map.get(ext) getOrElse ContentType.Bin

          val r = Responses.producerResponse(input, size).withMime(mime)

          resp(r.cache(240, lastMod.toString))
      }
    }
  }
}


