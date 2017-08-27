package net.shift.server.http

import net.shift.common.{Config, Path}
import net.shift.io.LocalFileSystem
import net.shift.server.{HttpServer, Server, ServerConfigNames}
import org.apache.log4j.BasicConfigurator

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  HttpServer(80, 10, req => resp =>
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

