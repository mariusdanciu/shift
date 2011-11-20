package net.shift
package common

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

}
