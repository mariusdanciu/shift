package net.shift
package engine.utils

import engine.ShiftApplication._
import engine.http._
import HttpPredicates._
import scalax.io._
import common._
import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.Try

object ShiftUtils {

  def cssFromFolder(folder: Path) = for {
    Path("styles" :: file :: _) <- path
    input <- fileOf(folder + file)
  } yield service(resp => resp(new CSSResponse(input)))

  def jsFromFolder(folder: Path) = for {
    Path("scripts" :: file :: _) <- path
    input <- fileOf(folder + file)
  } yield service(resp => resp(new JSResponse(input)))

  def imagesFromFolder(folder: Path) = for {
    Path("images" :: (f @ FileSplit(name, ext)) :: _) <- path
    input <- fileOf(folder + f)
  } yield service(resp => resp(new ImageResponse(input, "image/" + ext)))

  def fromFile(path: Path): Try[Input] =
    Try(Resource.fromInputStream(new BufferedInputStream(new FileInputStream(path toString))))
}