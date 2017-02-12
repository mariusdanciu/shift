package net.shift
package common

import net.shift.io.{BinProducer, IO}

import scala.io.Source
import scala.util.Try

object StringUtils {

  def toInt(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: NumberFormatException => None
  }

  def toInt(s: String, d: Int): Int = try {
    s.toInt
  } catch {
    case e: NumberFormatException => d
  }

  def toLong(s: String): Option[Long] = try {
    Some(s.toLong)
  } catch {
    case e: NumberFormatException => None
  }

  def toLong(s: String, d: Long): Long = try {
    s.toLong
  } catch {
    case e: NumberFormatException => d
  }

  def toFloat(s: String): Option[Float] = try {
    Some(s.toFloat)
  } catch {
    case e: NumberFormatException => None
  }

  def toFloat(s: String, d: Float): Float = try {
    s.toFloat
  } catch {
    case e: NumberFormatException => d
  }

  def toDouble(s: String): Option[Double] = try {
    Some(s.toDouble)
  } catch {
    case e: NumberFormatException => None
  }

  def toDouble(s: String, d: Double): Double = try {
    s.toDouble
  } catch {
    case e: NumberFormatException => d
  }

  def toBool(s: String): Option[Boolean] = try {
    Some(s.toBoolean)
  } catch {
    case e: Exception => None
  }

  def toBool(s: String, d: Boolean): Boolean = try {
    s.toBoolean
  } catch {
    case e: Exception => d
  }

  def load(resource: BinProducer): Try[String] = IO.producerToArray(resource).map { arr => new String(arr, "UTF-8") }

  def load(path: Path): Try[String] = Try(Source.fromFile(path.toString()).getLines().mkString)

  def load(in: Array[Byte]): Try[String] = Try(new String(in, "UTF-8"))

}
