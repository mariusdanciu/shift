package net.shift
package io

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.OutputStream
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.Exception._
import net.shift.common.Path
import java.io.FileInputStream
import java.io.BufferedInputStream

trait FileSystem {
  def exists(in: Path): Try[Boolean]
  def isDirectory(in: Path): Try[Boolean]
  def ls(in: Path): Try[List[Path]]
  def mkdir(p: Path): Boolean
  def deletePath(in: Path): Try[Path]
  def writer(p: Path): Iteratee[Array[Byte], Path]
  def reader(p: Path, bufSize: Int = 32768): Try[BinProducer]
  def lastModified(p: Path): Try[Long]
}

object LocalFileSystem extends FileSystem {

  def exists(in: Path): Try[Boolean] = Try {
    new File(in.toString).exists
  }

  def isDirectory(in: Path): Try[Boolean] = Try(new File(in.toString).isDirectory())

  def ls(in: Path): Try[List[Path]] =
    Try(new File(in.toString).listFiles().map(p => Path(p.getName)).toList)

  def mkdir(p: Path): Boolean = new File(p.toString).mkdirs()

  def deletePath(in: Path): Try[Path] = {

    def del(p: Path) = {
      val s = in.toString()
      if (new File(s).delete()) Success(p) else Failure(new FileNotFoundException(s))
    }

    val strIn = in.toString()
    isDirectory(in) match {
      case Success(true) =>
        ls(in) match {
          case Success(Nil) =>
            del(in)
          case Success(l) =>

            val res = for {
              n <- l
            } yield {
              deletePath(Path(s"$in/$n"))
            }

            res.find(_ isFailure) match {
              case Some(p) => p
              case _ =>
                deletePath(Path(s"$in")) flatMap { Success(_) }
            }

          case Failure(t) => Failure(t)
        }
      case _ =>
        catching(classOf[Exception]).withApply[Try[Path]](e => Failure(e)) {
          del(in)
        }
    }
  }

  def writer(p: Path): Iteratee[Array[Byte], Path] = IO failover {
    mkdir(p.dropLast)
    for {
      out <- Iteratee.foldLeft[Array[Byte], OutputStream](new BufferedOutputStream(new FileOutputStream(p.toString))) {
        (acc, e) => { acc.write(e); acc }
      }
    } yield {
      IO close out
      p
    }
  }

  def reader(p: Path, bufSize: Int = 32768): Try[BinProducer] =
    try {
      Success(IO inputStreamProducer (new BufferedInputStream(new FileInputStream(p.toString()), bufSize), bufSize))
    } catch {
      case e: Exception => Failure(e)
    }

  def lastModified(p: Path): Try[Long] = Try {
    new File(p.toString()).lastModified()
  }
}