package net.shift
package engine.utils

import engine.ShiftApplication._
import engine.http._
import scalax.io._
import common._
import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.Try

trait ShiftUtils extends HttpPredicates {

  def imageExtensions = Set("png", "jpg", "jpeg", "gif", "tif")

  def staticFiles(folder: Path) = for {
    Path("static" :: path) <- path
    input <- fileOf(folder + path.mkString("/", "/", ""))
  } yield {
    val FileSplit(name, ext) = path.last
    if (ext == "css") service(resp => resp(new CSSResponse(input)))
    else if (imageExtensions.contains(ext)) service(resp => resp(new ImageResponse(input, "image/" + ext)))
    else if (ext == "js") service(resp => resp(new JSResponse(input)))
    else if (ext == "html") service(resp => resp(new HtmlStaticResponse(input)))
    else service(resp => resp(new TextResponse(input)))
  }

}