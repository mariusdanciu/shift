package net.shift.common

object Base64 {
  val base64chars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  val encodeData = new Array[Byte](64)
  for (i <- 0 until 64) {
    encodeData(i) = base64chars.charAt(i).toByte
  }

  def unapply(msg: String): Option[String] = Some(new String(decode(msg), "UTF-8"))

  def encodeString(src: String) = encode(src.getBytes("UTF-8"))

  def encode(src: Array[Byte]): String = {
    val dst = new Array[Byte]((src.length + 2) / 3 * 4 + src.length / 72)
    var dstIndex = 0
    var state = 0
    var old = 0
    var srcIndex = 0
    for (x <- src) {
      state += 1
      state match {
        case 1 =>
          dst(dstIndex) = encodeData((x >> 2) & 0x3f)
        case 2 =>
          dst(dstIndex) = encodeData(((old << 4) & 0x30) | ((x >> 4) & 0xf))
        case 3 =>
          dst(dstIndex) = encodeData(((old << 2) & 0x3C) | ((x >> 6) & 0x3))
          dstIndex += 1
          dst(dstIndex) = encodeData(x & 0x3F)
          state = 0
      }
      dstIndex += 1
      old = x;
      if (srcIndex >= 72) {
        dst(dstIndex) = '\n'.toByte
        dstIndex += 1
      }
      srcIndex
    }

    state match {
      case 1 =>
        dst(dstIndex) = encodeData((old << 4) & 0x30)
        dst(dstIndex + 1) = '='.toByte;
        dst(dstIndex + 2) = '='.toByte
      case 2 =>
        dst(dstIndex) = encodeData((old << 2) & 0x3c);
        dst(dstIndex + 1) = '='.toByte
    }
    new String(dst)
  }

  def decodeString(s: String) = {
    new String(decode(s), "UTF-8")
  }

  def decode(s: String): Array[Byte] = {
    var end = if (s.endsWith("==")) {
      2
    } else if (s.endsWith("=")) {
      1
    } else 0

    val len = (s.length() + 3) / 4 * 3 - end;
    val result = new Array[Byte](len);
    var dst = 0;

    var code = 0
    for (src <- 0 until s.length if (code != -1 && dst < len)) {
      code = base64chars.indexOf(s.charAt(src));
      (src % 4) match {
        case 0 =>
          result(dst) = (code << 2).toByte
        case 1 =>
          result(dst) = (result(dst) | ((code >> 4) & 0x3).toByte).toByte
          dst += 1
          if (dst < len)
            result(dst) = (code << 4).toByte
        case 2 =>
          result(dst) = (result(dst) | ((code >> 2) & 0xf).toByte).toByte
          dst += 1
          if (dst < len)
            result(dst) = (code << 6).toByte
        case 3 =>
          result(dst) = (result(dst) | (code & 0x3f).toByte).toByte
          if (dst < len)
            dst += 1
      }
    }
    result
  }

}