package net.shift
package engine.utils

import scala.util.Try

import common._
import engine.ShiftApplication._
import engine.http._
import net.shift.io.FileSystem
import engine.http.Response._

trait ShiftUtils extends HttpPredicates {

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
          else if (ext == "js") new JsResponse(input)
          else if (ext == "html") HtmlStaticResponse(input)
          else TextResponse(input)

          resp(r.cache(240, lastMod.toString))
      }
    }

  }

}