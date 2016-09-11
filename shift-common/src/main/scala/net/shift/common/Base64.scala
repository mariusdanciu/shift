package net.shift.common

import scala.annotation.tailrec
import net.shift.security.HMac
import net.shift.security.User
import net.shift.security.Permission

object Base64 {
  val base64chars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def unapply(msg: String): Option[String] = 
    if (msg == "") None else Some(new String(decode(msg), "UTF-8"))

  def encodeString(src: String) = encode(src.getBytes("UTF-8"))

  def encode(src: Array[Byte]): String = {
    def encodeAtom(b0: Byte, b1: Byte, b2: Byte): (Char, Char, Char, Char) = {
      val v: Integer = ((b0 << 16) & 0x00ff0000) + ((b1 << 8) & 0x0000ff00) + (b2 & 0x000000ff)

      (base64chars.charAt((v >> 18).toInt),
        base64chars.charAt(((v & 0x3F000) >> 12).toInt),
        base64chars.charAt(((v & 0xFC0) >> 6).toInt),
        base64chars.charAt((v & 0x3F).toInt))
    }

    @tailrec
    def walk(start: Int, end: Int, src: Array[Byte], dst: String): String = {
      if (start < end) {
        val (c0, c1, c2, c3) = encodeAtom(src(start), src(start + 1), src(start + 2))
        val acc = dst + c0 + c1 + c2 + c3
        walk(start + 3, end, src, acc)
      } else {
        dst
      }
    }

    var dst = ""

    src.length match {
      case 0 => ""
      case 1 =>
        val (c0, c1, c2, c3) = encodeAtom(src(0), 0, 0)
        dst + c0 + c1 + "=="
      case 2 =>
        val (c0, c1, c2, c3) = encodeAtom(src(0), src(1), 0)
        dst + c0 + c1 + c2 + "="
      case len =>
        val rest = len % 3
        dst = walk(0, len - rest, src, "")
        if (rest != 0) {
          dst + encode(src.slice(len - rest, len))
        } else
          dst
    }

  }

  def decodeString(s: String) = {
    new String(decode(s), "UTF-8")
  }

  def decode(s: String): Array[Byte] = {
    def dencodeAtom(c0: Char, c1: Char, c2: Char, c3: Char): (Byte, Byte, Byte) = {
      val v: Long = (base64chars.indexOf(c0).toLong << 18) +
        (base64chars.indexOf(c1).toLong << 12) +
        (if (c2 != '=') base64chars.indexOf(c2).toLong << 6 else 0) +
        (if (c3 != '=') base64chars.indexOf(c3) else 0)

      ((v >> 16).toByte, ((v & 0xff00) >> 8).toByte, ((v & 0xff)).toByte)
    }
    var l: List[Byte] = Nil
    for { b <- 0 until s.length() by 4 } {
      l ++= (dencodeAtom(s.charAt(b), s.charAt(b + 1), s.charAt(b + 2), s.charAt(b + 3)) match {
        case (b0, 0, 0)   => List(b0)
        case (b0, b1, 0)  => List(b0, b1)
        case (b0, b1, b2) => List(b0, b1, b2)
      })
    }
    l.toArray
  }

}
