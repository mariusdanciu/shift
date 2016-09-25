package net.shift
package common

import java.nio.ByteBuffer
import scala.util.Success
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import io.IO._
import net.shift.io.Cont
import net.shift.io.Data
import net.shift.io.Done
import net.shift.io.EOF
import net.shift.io.IO
import net.shift.io.IterateeProducer._
import net.shift.io.LocalFileSystem._
import net.shift.io.LocalFileSystem
import net.shift.io.LocalFileSystem
import java.nio.channels.SocketChannel
import java.io.IOException
import net.shift.io.Iteratee

trait UnitTest extends FlatSpec with Matchers

class IOTest extends UnitTest {

  implicit val fs = LocalFileSystem
  implicit def string2Iterable(in: String) =
    in.map(c => Data(ByteBuffer.wrap(Array[Byte](c.toByte)))) ++ List(EOF)

  val str = "This is a text."

  "IO" should "read/write correctly" in {
    traverse(str)(writer(Path("test.txt")))

    val res = for {
      r <- reader(Path("test.txt"))
      s <- IO producerToString r
    } yield {
      println("s = " + s)
      s
    }

    res should equal(Success(str))
  }

  "File" should "delete correctly" in {
    deletePath(Path("test.txt"))
  }

  "Path" should "delete correctly" in {
    mkdir(Path("a/b/c/d"))
    mkdir(Path("a/b/c/e"))
    deletePath(Path("a")) should equal(Success(Path("a")))
    exists(Path("a")) should equal(Success(false))
  }

  "Path" should "behave correctly" in {
    val p = Path("c:/a/b/c")
    p should equal(Path("c:/a/b/c"))
    p.toString should equal("c:/a/b/c")

    Path("/a/b/c").toString should equal("/a/b/c")
    Path("c:/a/b/c").scheme should equal(Some("c"))
    Path("a/b/c/") match {
      case Path(None, "a" :: "b" :: "c" :: "" :: Nil) => println("OK")
      case _ => fail("a/b/c did no extract correctly")
    }

    Path("c:/a/b/c") match {
      case Path(Some("c"), "" :: "a" :: "b" :: "c" :: Nil) => println("OK")
      case _ => fail("c:/a/b/c did no extract correctly")
    }

    Path("c:a/b/c") match {
      case EmptyPath => println("OK")
      case _         => fail("c:a/b/c should be invalid")
    }

  }

  "IO" should "read/write large files" in {
    val file = IO.fileProducer(Path("./src/test/resources/vopsea.png"), 32768)

    var totalSize = 0

    def write(b: ByteBuffer): Int = {
      val size = if (b.remaining() < 1024) b.remaining() else 1024
      val arr = Array.fill[Byte](size)(0)
      val oldP = b.position()
      val res = b.get(arr)
      val newP = b.position()
      println("remain " + b.remaining())
      println("wrote " + b + " - " + (newP - oldP))
      totalSize += (newP - oldP)
      res.position
    }

    def cont: Iteratee[ByteBuffer, Unit] = Cont {
      case Data(d) =>

        val wrote = write(d)

        wrote match {
          case 0 =>
            Done((), Data(d))
          case -1 => net.shift.io.Error[ByteBuffer, Unit](new IOException("Client connection closed."))
          case _  => cont
        }
      case EOF =>
        Done((), EOF)
    }

    println(file map { f => f._2(cont) })
    assert(totalSize == 276405)

  }

}