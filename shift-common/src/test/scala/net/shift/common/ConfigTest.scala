package net.shift.common

import net.shift.io.LocalFileSystem

import scala.util.{Failure, Success}

/**
  * Created by mariu on 7/2/2017.
  */
class ConfigTest extends UnitTest {

  implicit val fs = LocalFileSystem


  "Config" should "parse correctly" in {

    val r = Config.fromString(
      """
        | prop1 = val1 an then some text
        |
        | prop2 = val2
        | prop3 = val3
        |
        | prop4 = val4
        |
        | seg1 {
        |   prop2 = val2
        |   prop3 = val3
        |
        |   seg2 {
        |     prop2 = val2
        |     prop3 = val3
        |   }
        |
        |   prop5 = val5
        | }
        |
        |  prop5 = val5
        |
        |
    """.stripMargin)

    r match {
      case Success(c) =>
        println(c.configs)
        assert(c.configs.size === 10)
      case Failure(t) => fail(t)
    }
  }

}
