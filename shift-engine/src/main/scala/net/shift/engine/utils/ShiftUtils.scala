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
    Path(_, "static" :: p) <- path
    input <- fileOf(folder + p.mkString("/"))
  } yield {
    val FileSplit(name, ext) = p.last
    if (ext == "css") service(resp => resp(new CSSResponse(input)))
    else if (imageExtensions.contains(ext)) service(resp => resp(new ImageResponse(input, "image/" + ext).withHeaders(Header("cache-control", "max-age=86400"))))
    else if (ext == "js") service(resp => resp(new JsResponse(input)))
    else if (ext == "html") service(resp => resp(new HtmlStaticResponse(input)))
    else service(resp => resp(new TextResponse(input)))
  }

}