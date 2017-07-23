package net.shift.engine

import net.shift.common.{Config, State}
import net.shift.engine.RoutesImplicits._
import net.shift.engine.http.HttpPredicates._
import net.shift.io.LocalFileSystem
import net.shift.server.{HttpServer, HttpsServer}
import net.shift.server.http.{Request, Responses}
import org.apache.log4j.BasicConfigurator

/**
  * Created by Marius Danciu on 7/11/2017.
  */
object Test extends App {
  implicit val fs = LocalFileSystem

  BasicConfigurator.configure


  implicit val config = Config fromString
    """
      |server {
      |  address = 0.0.0.0
      |  port = 80
      |  numThreads = 50
      |
      |  ssl {
      |    port = 443
      |    numThreads = 30
      |    keystore = .keystore
      |    truststore = .truststore
      |    pass = idid.1
      |  }
      |}
    """.stripMargin get



  HttpServer(config, TestApp.shiftService).start()
  HttpsServer(config, TestApp.shiftService).start()
}

import net.shift.engine.ShiftApplication._

object TestApp extends ShiftApplication {
  override def servingRule: State[Request, Attempt] =
    endpoint1 |
      notFound

  def notFound = serve(Responses.textResponse(s"BAD REQUEST"))

  def endpoint1 = for {
    _ <- get
    (i, s) <- emptyPart / "a" / IntPart / "b" / StringPart extract
  } yield {
    serve(Responses.textResponse(s"got it. $i $s"))
  }
}
