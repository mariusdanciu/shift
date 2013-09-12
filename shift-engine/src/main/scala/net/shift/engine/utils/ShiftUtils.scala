package net.shift
package engine.utils

import engine.ShiftApplication._
import engine.http._
import HttpPredicates._
import scalax.io._

object ShiftUtils {

  def cssFromFolder(folder: String) = for {
    "styles" :: file :: _ <- path
  } yield service(resp => resp(new CSSResponse(Resource.fromFile(folder + "/" + file))))

  def jsFromFolder(folder: String) = for {
    "scripts" :: file :: _ <- path
  } yield service(resp => resp(new JSResponse(Resource.fromFile(folder + "/" + file))))

  def jpgFromFolder(folder: String) = for {
    "images" :: file :: _ <- path
  } yield service(resp => resp(new ImageResponse(Resource.fromFile(folder + "/" + file), "image/jpg")))

}