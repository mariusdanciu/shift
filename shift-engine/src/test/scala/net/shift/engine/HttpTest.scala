package net.shift
package engine

import org.scalatest._
import net.shift.engine.http.MultipartParser
import net.shift.engine.http.BinReader
import net.shift.engine.http.TextPart
import net.shift.engine.http.BinaryPart

class HttpTest extends FlatSpec with Matchers {

  "Multipart" should "parse" in {
    test1
  }

  def test1 = {

    val bin = Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    val body = "------WebKitFormBoundaryePkpFF7tjBAqx29L\r\n\r\n------WebKitForm\r\n------WebKitFormBoundaryePkpFF7tjBAqx29L\r\nContent-Disposition: form-data; name=\"MAX_FILE_SIZE\"\r\n\r\n100000\r\n------WebKitFormBoundaryePkpFF7tjBAqx29L\r\nContent-Disposition: form-data; name=\"uploadedfile\"; filename=\"hello.o\"\r\nContent-Type: application/x-object\r\n\r\n".getBytes("UTF-8") ++
      bin ++ "\r\n------WebKitFormBoundaryePkpFF7tjBAqx29L--".getBytes("UTF-8")

      
    val parser = new MultipartParser("----WebKitFormBoundaryePkpFF7tjBAqx29L".getBytes("UTF-8"))

    parser.multiParser(BinReader(body, 0)) map { v =>
      println(v)
      v.parts.map {
        _ match {
          case BinaryPart(h, c) =>
            val value = c.map(String.valueOf(_)).mkString
            println(value)
            //assert(c === bin)
          case TextPart(h, c) =>
            println(c)
            //assert(c === "100000")
          case _ =>
        }
      }
    }
  }
}

object Run extends App {
  new HttpTest().test1
}
