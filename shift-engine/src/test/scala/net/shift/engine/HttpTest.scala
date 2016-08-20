package net.shift
package engine

import org.scalatest._
import net.shift.engine.http.MultipartParser
import net.shift.engine.http.BinReader
import net.shift.engine.http.TextPart
import net.shift.engine.http.BinaryPart
import net.shift.io.LocalFileSystem
import net.shift.common.Path
import net.shift.io.IO
import net.shift.io.LocalFileSystem

class HttpTest extends FlatSpec with Matchers {

  def test1 = {

    val bin = Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    val body = "------WebKitFormBoundaryePkpFF7tjBAqx29L\r\n\r\n------WebKitForm\r\n------WebKitFormBoundaryePkpFF7tjBAqx29L\r\nContent-Disposition: form-data; name=\"MAX_FILE_SIZE\"\r\n\r\n<span style=\"color:rgb(0,0,255);\">Cea mai tare <b style=\"color:rgb(0,0,255);\">soseta</b> din Ardeal.</span><p></p>\r\n------WebKitFormBoundaryePkpFF7tjBAqx29L\r\nContent-Disposition: form-data; name=\"uploadedfile\"; filename=\"hello.o\"\r\nContent-Type: application/x-object\r\n\r\n".getBytes("UTF-8") ++
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

  def test2 = {
    val body = "-----------------------------902056036781473372360087476\r\nContent-Disposition: form-data; name=\"create_title\"\r\n\r\nSosetuta\r\n-----------------------------902056036781473372360087476\r\nContent-Disposition: form-data; name=\"create_price\"\r\n\r\n44\r\n-----------------------------902056036781473372360087476\r\nContent-Disposition: form-data; name=\"create_categories\"\r\n\r\n5479c53fe4b04cb784a98b8f\r\n-----------------------------902056036781473372360087476\r\nContent-Disposition: form-data; name=\"create_keywords\"\r\n\r\ns\r\n-----------------------------902056036781473372360087476\r\nContent-Disposition: form-data; name=\"create_description\"\r\n\r\n<span style=\"color:rgb(0,0,255);\">Cea mai tare <b style=\"color:rgb(0,0,255);\">soseta</b> din Ardeal.</span><p></p>\r\n-----------------------------902056036781473372360087476--"
    val parser = new MultipartParser("---------------------------902056036781473372360087476".getBytes("UTF-8"))

    parser.multiParser(BinReader(body.getBytes("utf-8"), 0)) map { v =>
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

  def test3 = {

    val v = for {
      prod <- LocalFileSystem.reader(Path("c:/work/upload-1442129190413.bin"))
      array <- IO.toArray(prod)
    } yield {
      val parser = new MultipartParser("---------------------------3357733724543".getBytes("UTF-8"))
      parser.multiParser(BinReader(array)) map { v =>
        v.parts.map {
          _ match {
            case b @ BinaryPart(h, c) =>
              println("Binary : " + h + " ... " + c.length)
              b
            //assert(c === bin)
            case b @ TextPart(h, c) =>
              println("Text : " + c)
              b
            case _ =>
          }
        }
      }
    }
    println(v)
  }
}

object Run extends App {
  new HttpTest().test2
}
