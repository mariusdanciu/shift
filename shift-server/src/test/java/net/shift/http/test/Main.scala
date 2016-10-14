package net.shift.http.test

import net.shift.common.Path
import net.shift.io.LocalFileSystem
import net.shift.server.ServerSpecs
import net.shift.server.Server
import org.apache.log4j.BasicConfigurator
import net.shift.http.Responses
import net.shift.http.HttpProtocol
import net.shift.http.Get
import net.shift.http._

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  Server(ServerSpecs("test",
    "0.0.0.0",
    8081,
    3)).start(HttpProtocol(req => resp =>

    if (req.uri.path == "/pic")
      Responses.imageFileResponse(Path("./src/test/resources/pic.jpg")) map { resp }
    else
      resp(Responses.textResponse("Got it"))))

}


