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

  val config = Config(
    "ssl.address" -> "0.0.0.0",
    "ssl.port" -> "443",
    "ssl.numThreads" -> "10",
    "ssl.keystore" -> ".keystore",
    "ssl.truststore" -> ".truststore",
    "ssl.pass" -> "idid.1"
  )


  HttpsServer(config, req => resp => {

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
