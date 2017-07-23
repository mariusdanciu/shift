package net.shift.server.http

import net.shift.common.{Config, Path}
import net.shift.io.LocalFileSystem
import net.shift.server.{HttpServer, Server}
import org.apache.log4j.BasicConfigurator

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  val config = Config(
    "server.address" -> "0.0.0.0",
    "server.port" -> "80",
    "server.numThreads" -> "10"
  )

  HttpServer(config, req => resp =>
    if (req.uri.path == "/pic") {
      val r = Responses.imageFileResponse(Path("./shift-server/src/test/resources/pic.jpg"))
      r map {
        resp
      }
    } else if (req.uri.path == "/cart") {
      resp(Responses.textResponse("got cart"))
    } else
      resp(Responses.textResponse("Got it"))).start()

}

