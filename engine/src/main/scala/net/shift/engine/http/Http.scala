
package net.shift
package engine
package http

import java.io.{InputStream, OutputStream}
import java.net.URL

trait ReadChannel {
  def readBuffer(buf: Array[Byte]): Int
  def readInt : Int
  def readByte: Byte
  def readLong: Long
  def readFloat: Float
  def readDouble: Double
  def readShort: Short
  def readBoolean: Boolean
  def readChar: Char
}

trait WriteChannel {
  def writeBuffer(buf: Array[Byte])
  def writeInt(v: Int)
  def writeByte(v: Byte)
  def writeLong(v: Long)
  def writeFloat(v: Float)
  def writeDouble(v: Double)
  def writeShort(v: Short)
  def writeBoolean(v: Boolean)
  def writeChar(v: Char)
  def bytesWritten: Long
 }

trait Request {
  def path: String
  def method : String
  def contextPath : String
  def queryString: Option[String]
  def param(name: String): List[String]
  def params: Map[String, List[String]]
  def header(name: String): Option[String]
  def headers: Map[String, String]
  def contentLength: Option[Long]
  def contentType: Option[String]
  def cookies: Map[String, Cookie]
  def cookie(name: String): Option[Cookie]
  def readBody: ReadChannel
}

trait Response {
  def code: Int
  def reason: String
  def headers: Map[String, String]
  def contentType: Option[String]
  def cookies: List[Cookie]
  def writeBody(channel: WriteChannel)
}

trait Context {
  def resourceAsStream(res: String): Option[InputStream]
  def contextPath: String
}


object Cookie {
  def apply(name: String, value: String) =
    new Cookie(name, value, None, None, None, None, false, false)

  def apply(name: String, value: String, maxAge: Int) =
    new Cookie(name, value, None, None, Some(maxAge), None, false, false)
}

case class Cookie(name: String,
                  value: String,
                  domain: Option[String],
                  path: Option[String],
                  maxAge: Option[Int],
                  version: Option[Int],
                  secure : Boolean,
                  httpOnly: Boolean)

trait HttpMethod
case object GET extends HttpMethod
case object POST extends HttpMethod
case object PUT extends HttpMethod
case object DELETE extends HttpMethod
case object HEAD extends HttpMethod
