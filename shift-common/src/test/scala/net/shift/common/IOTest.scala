package net.shift
package common

import org.scalatest.Inside
import org.scalatest.Inspectors
import org.scalatest.OptionValues
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import io.IO._
import net.shift.io.IterateeProducer._
import net.shift.io.EOF
import net.shift.io.Data
import net.shift.io.FileOps._
import scala.util.Success
import net.shift.io.IO

trait UnitTest extends FlatSpec with Matchers

class IOTest extends UnitTest {

  implicit def string2Iterable(in: String) =
    in.map(c => Data(Array[Byte](c.toByte))) ++ List(EOF)

  val str = "This is a text."
  "IO" should "read/write correctly" in {
    traverse(str)(writer(Path("test.txt")))

    val res = for {
      r <- reader(Path("test.txt"))
      s <- IO toString r
    } yield s

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
      case _                                    => fail("a/b/c did no extract correctly")
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

}