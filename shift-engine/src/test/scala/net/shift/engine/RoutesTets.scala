package net.shift.engine

import net.shift.engine.RoutesImplicits._
import net.shift.engine.ShiftApplication._
import net.shift.engine.http.HttpPredicates.get
import net.shift.server.http.Responses._
import net.shift.server.http.{Request, Uri, Ver}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by mariu on 7/8/2017.
  */
class RoutesTets extends FlatSpec with Matchers {

  "Typesafe routes" should "work" in {
    val res2 = for {
      _ <- get
      t @ (l, i1, i2, d, i3) <- emptyPart / "a" / ListPart(2) / "b" / IntPart / IntPart / DoublePart / IntPart extract
    } yield {
      println(s"Extracted $t")
      serve(ok)
    }

    val app = res2(Request(
      "GET",
      Uri("/a/123/s/b/2/5/3.14/10"),
      Ver.Ver_1_1,
      Nil,
      null))

    println(app)
    assert(app.isSuccess)
  }

}
