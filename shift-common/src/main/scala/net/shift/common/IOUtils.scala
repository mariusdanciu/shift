package net.shift
package common

import net.shift.io.ReadChannel
import java.io.InputStream
import java.io.DataInputStream

object IOUtils {

  implicit def ch2InputStream(r: ReadChannel): InputStream = new InputStream {
    override def read(): Int = r.readByte
    override def read(buf: Array[Byte]): Int = r readBuffer buf
    override def read(buf: Array[Byte], offset: Int, length: Int): Int = r readBuffer (buf, offset, length)
    override def close = r close
  }

  implicit def inputStream2Ch(in: InputStream): ReadChannel = { 
    val dis = new DataInputStream(in)
    new ReadChannel {
      def readBuffer(buf: Array[Byte]): Int = dis read buf
      def readBuffer(buf: Array[Byte], offset: Int, length: Int): Int = dis read (buf, offset, length)
      def readInt: Int = dis read
      def readByte: Byte = dis readByte
      def readLong: Long = dis readLong
      def readFloat: Float = dis readFloat
      def readDouble: Double = dis readDouble
      def readShort: Short = dis readShort
      def readBoolean: Boolean = dis readBoolean
      def readChar: Char = dis readChar
      def close = dis close
    }
  }
  
  def safe[T](cls : io.Closing*)(f : => T): T = {
    try {
      f
    } finally {
      cls map {_ close}
    }
  }
  
}