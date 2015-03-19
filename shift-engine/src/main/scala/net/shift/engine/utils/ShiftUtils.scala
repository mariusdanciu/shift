package net.shift
package engine.utils

import engine.ShiftApplication._
import engine.http._
import common._
import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.Try
import net.shift.io.FileSystem

trait ShiftUtils extends HttpPredicates {

  def imageExtensions = Set("png", "jpg", "jpeg", "gif", "tif")

  def staticFiles(folder: Path)(implicit fs: FileSystem) = for {
    Path(_, "static" :: p) <- path
    input <- fileOf(folder + p.mkString("/"))
  } yield {
    val FileSplit(name, ext) = p.last
    if (ext == "css") service(resp => resp(new CSSResponse(input)))
    else if (imageExtensions.contains(ext)) service(resp => resp(new ImageResponse(input, "image/" + ext)))
    else if (ext == "js") service(resp => resp(new JsResponse(input)))
    else if (ext == "html") service(resp => resp(new HtmlStaticResponse(input)))
    else service(resp => resp(new TextResponse(input)))
  }

}