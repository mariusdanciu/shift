package net.shift.server.http

import net.shift.common.Path
import net.shift.io.LocalFileSystem
import net.shift.server.{Server, ServerConfig}
import org.apache.log4j.BasicConfigurator

object Main extends App {

  implicit val fs = LocalFileSystem

  BasicConfigurator.configure

  Server(ServerConfig("test",
    "0.0.0.0",
    80,
    3)).start(HttpProtocolBuilder(req => resp =>

    if (req.uri.path == "/pic") {
      val r = Responses.imageFileResponse(Path("./shift-server/src/test/resources/pic.jpg"))
      r map { resp }
    } else
      resp(Responses.textResponse("Got it"))))

}


