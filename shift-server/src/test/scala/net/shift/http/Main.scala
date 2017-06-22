package net.shift.server.http

import org.apache.log4j.BasicConfigurator
import net.shift.server.http._
import net.shift.io.LocalFileSystem
import net.shift.server.Server
import net.shift.server.ServerSpecs
import net.shift.common.Path

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  Server(ServerSpecs("test",
    "0.0.0.0",
    8081,
    3)).start(HttpProtocolBuilder(req => resp =>

    if (req.uri.path == "/pic")
      Responses.imageFileResponse(Path("./src/test/resources/pic.jpg")) map { resp }
    else
      resp(Responses.textResponse("Got it"))))

}


