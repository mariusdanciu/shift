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

trait UnitTest extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

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

}