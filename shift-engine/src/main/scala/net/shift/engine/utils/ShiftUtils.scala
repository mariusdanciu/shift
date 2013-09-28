package net.shift
package engine.utils

import engine.ShiftApplication._
import engine.http._
import HttpPredicates._
import scalax.io._
import common._

object ShiftUtils {

  def cssFromFolder(folder: String) = for {
    "styles" :: file :: _ <- path
  } yield service(resp => resp(new CSSResponse(Resource.fromFile(folder + "/" + file))))

  def jsFromFolder(folder: String) = for {
    "scripts" :: file :: _ <- path
  } yield service(resp => resp(new JSResponse(Resource.fromFile(folder + "/" + file))))

  def imagesFromFolder(folder: String) = for {
     "images" :: (f @ FileSplit(name, ext)) :: _ <- path
  } yield service(resp => resp(new ImageResponse(Resource.fromFile(folder + "/" + f), "image/" + ext)))

}