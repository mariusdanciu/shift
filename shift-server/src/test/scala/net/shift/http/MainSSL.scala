package net.shift.server.http

import net.shift.common.{Config, Path}
import net.shift.io.LocalFileSystem
import net.shift.server._
import org.apache.log4j.BasicConfigurator

/**
  * Created by mariu on 1/5/2017.
  */
object MainSSL extends App {

  implicit val fs = LocalFileSystem

  println("Starting ")
  BasicConfigurator.configure
  System.setProperty("https.protocols", "TLSv1");

  HttpsServer(443, 10, "idid.1", req => resp => {

    println("Got request " + req)

    if (req.uri.path == "/pic") {
      val r = Responses.imageFileResponse(Path("./shift-server/src/test/resources/pic.jpg"))
      println(r)
      r map {
        resp
      }
    } else
      resp(Responses.textResponse("Got it"))
  }
  ).start()

}
