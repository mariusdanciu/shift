package test

import net.shift.common.Path
import net.shift.io.LocalFileSystem
import net.shift.server.{SSLServer, SSLServerSpecs, Server, ServerSpecs}
import net.shift.server.http.{HttpProtocolBuilder, Responses}
import org.apache.log4j.BasicConfigurator

/**
  * Created by mariu on 1/5/2017.
  */
object MainSSL extends App {

  implicit val fs = LocalFileSystem

  println("Starting ")
  BasicConfigurator.configure
  System.setProperty("https.protocols", "TLSv1");

  SSLServer(SSLServerSpecs("test",
    "0.0.0.0",
    443,
    30,
    ".keystore",
    ".truststore",
    "idid.1"
  )).start(HttpProtocolBuilder(req => resp => {

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
  ))

}