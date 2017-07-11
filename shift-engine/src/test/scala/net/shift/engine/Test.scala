package net.shift.engine

import net.shift.common.{Config, Path, State}
import net.shift.io.LocalFileSystem
import net.shift.server.{Server, ServerConfig}
import net.shift.server.http.{HttpProtocolBuilder, Request, Responses}
import org.apache.log4j.BasicConfigurator
import net.shift.engine.http.HttpPredicates._
import RoutesImplicits._

/**
  * Created by Marius Danciu on 7/11/2017.
  */
object Test extends App {
  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  implicit val cfg = Config()

  Server(ServerConfig("test",
    "0.0.0.0",
    80,
    3)).start(HttpProtocolBuilder(TestApp.shiftService))
}

import ShiftApplication._

object TestApp extends ShiftApplication {
  override def servingRule: State[Request, Attempt] = endpoint1 |
    notFound

  def notFound = service(_ (Responses.textResponse(s"BAD REQUEST")))

  def endpoint1 = for {
    _ <- get
    (i, s) <- (/("a") / IntPart / "b" / StringPart) extract
  } yield {
    serve(Responses.textResponse(s"got it. $i $s"))
  }
}
