package net.shift.http.websockets

import scala.util.Try

import net.shift.common.BinReader
import net.shift.common.ShiftParsers
import java.nio.ByteBuffer

/*
 * Parser that parses a Web-Socket Frame
 *
 *  0                   1                   2                   3
 *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 * +-+-+-+-+-------+-+-------------+-------------------------------+
 * |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
 * |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
 * |N|V|V|V|       |S|             |   (if payload len==126/127)   |
 * | |1|2|3|       |K|             |                               |
 * +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
 * |     Extended payload length continued, if payload len == 127  |
 * + - - - - - - - - - - - - - - - +-------------------------------+
 * |                               |Masking-key, if MASK set to 1  |
 * +-------------------------------+-------------------------------+
 * | Masking-key (continued)       |          Payload Data         |
 * +-------------------------------- - - - - - - - - - - - - - - - +
 * :                     Payload Data continued ...                :
 * + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
 * |                     Payload Data continued ...                |
 * +---------------------------------------------------------------+
 */
class FrameParser extends ShiftParsers {

  def header: Parser[FrameHeader] = new Parser[FrameHeader] {
    def apply(in: Input): ParseResult[FrameHeader] = {

      def mask(maskFlag: Boolean, in: Input): (Array[Int], Input) = {
        var i = in
        if (maskFlag) {
          var value = new Array[Int](4)

          for { k <- 0 until 4 } yield {
            i = i.rest
            value(k) = i.first
          }
          (value, i.rest)
        } else {
          (new Array[Int](4), i)
        }
      }

      def length(in: Input): (Long, Input) = {
        var i = in
        val len1 = (in.first & 0x7f)

        val byteCount = if (len1 <= 125) {
          0
        } else if (len1 == 126) {
          2
        } else if (len1 == 127) {
          8
        } else {
          -1
        }

        if (byteCount == -1) {
          (-1, i)
        } else if (byteCount > 0) {
          i = i.rest
          var value: Long = i.first
          for { k <- 1 until byteCount } yield {
            i = i.rest
            value = (value << 8) + i.first
          }
          (value, i.rest)
        } else {
          (len1, i.rest)
        }
      }

      var i = in

      val b1 = in.first

      val fin = (b1 & 0x80) != 0
      val rsv1 = (b1 & 0x40) != 0
      val rsv2 = (b1 & 0x20) != 0
      val rsv3 = (b1 & 0x10) != 0

      val op = OpCode.fromInt(b1 & 0x0F)

      i = i.rest

      val maskFlag = (i.first & 0x80) != 0

      val (len, rest) = length(i)
      if (len == -1) {
        Failure("Invalid length", i)
      } else {
        val (maskKey, rest2) = mask(maskFlag, rest)
        Success(FrameHeader(fin, rsv1, rsv2, rsv3, op, maskFlag, len, maskKey), rest2)
      }
    }
  }

  def frame: Parser[Frame] = parserByPrevResults(header, (h: FrameHeader) => repN(h.length.toInt, byte) ^^ {
    case r => Frame(h, ByteBuffer.wrap(r.toArray))
  })

  def parse(reader: BinReader): Try[Frame] = {
    frame(reader) match {
      case Success(r, _) => scala.util.Success(r)
      case Failure(f, p) =>
        scala.util.Failure(new Exception(f))
      case Error(f, p) =>
        scala.util.Failure(new Exception(f))
    }
  }
}

object OpCode {
  def fromInt(v: Int): OpCode = v match {
    case 0  => Continuation
    case 1  => TextFrame
    case 2  => BinaryFrame
    case 8  => ConnectionClose
    case 9  => Ping
    case 10 => Pong
    case v  => Reserved(v)
  }
}

sealed trait OpCode {
  val code: Int
}
case object Continuation extends OpCode {
  val code = 0x0
}
case object TextFrame extends OpCode {
  val code = 0x1
}
case object BinaryFrame extends OpCode {
  val code = 0x2
}
case object ConnectionClose extends OpCode {
  val code = 0x8
}
case object Ping extends OpCode {
  val code = 0x9
}
case object Pong extends OpCode {
  val code = 0xA
}
case class Reserved(code: Int) extends OpCode

case class FrameHeader(fin: Boolean,
                       rsv1: Boolean,
                       rsv2: Boolean,
                       rsv3: Boolean,
                       opCode: OpCode,
                       mask: Boolean,
                       length: Long,
                       maskKey: Array[Int])
case class Frame(header: FrameHeader, msg: ByteBuffer)

