package net.shift.server.http

import net.shift.common.Path
import net.shift.io.LocalFileSystem
import net.shift.server.HttpServer
import org.apache.log4j.BasicConfigurator

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure


  HttpServer(80, 10, HttpService.build(req => resp => {
    if (req.uri.path == "/pic") {
      val r = Responses.imageFileResponse(Path("./shift-server/src/test/resources/pic.jpg"))
      r map { s =>
        println(resp(s))
        println(resp(s))
      }
    } else if (req.uri.path == "/cart") {
      resp(Responses.textResponse("got cart"))
    } else
      resp(Responses.textResponse("Got it"))
  })).start()

}

