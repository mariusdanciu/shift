package net.shift
package engine.utils

import scala.util.Try

import common._
import engine.ShiftApplication._
import engine.http._
import net.shift.io.FileSystem
import engine.http.Response._
import net.shift.security.HMac
import net.shift.security.Users
import net.shift.security.BasicCredentials
import net.shift.security.User
import net.shift.security.Credentials
import scala.util.Success

import HttpPredicates._

object ShiftUtils {

  def imageExtensions = Set("png", "jpg", "jpeg", "gif", "tif")

  def staticFiles(folder: Path)(implicit fs: FileSystem) = for {
    r <- req
    Path(_, "static" :: p) <- path
    val localPath = folder + p.mkString("/")
    input <- fileOf(localPath)
    lastMod <- lastModified(localPath)
  } yield {

    service { resp =>
      r.header("If-None-Match") match {
        case Some(Header(_, etag)) if (etag == lastMod.toString) =>
          resp(Resp.notModified)
        case _ =>
          val FileSplit(name, ext) = p.last
          val r = if (ext == "css") new CSSResponse(input)
          else if (imageExtensions.contains(ext)) new ImageResponse(input, "image/" + ext)
          else if (ext == "svg") new ImageResponse(input, "image/" + ext + "+xml")
          else if (ext == "js") new JsResponse(input)
          else if (ext == "html") HtmlStaticResponse(input)
          else TextResponse(input)

          resp(r.cache(240, lastMod.toString))
      }
    }

  }

  def computeUser(r: Request)(implicit login: Credentials => Try[User], conf: Config): Try[User] = {
    (r.header("Authorization"), r.cookie("identity"), r.cookie("secret")) match {
      case (Some(Authorization(creds @ BasicCredentials(user, password))), None, None) =>
        login(creds)
        
      case (_, Some(Cookie(_, Base64(identity), _, _, _, _, _, _)), Some(Cookie(_, secret, _, _, _, _, _, _))) =>
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

}