package net.shift.http

import net.shift.common.Path
import net.shift.io.{IO, LocalFileSystem}
import net.shift.server.http.HttpParser

/**
  * Created by mariu on 7/1/2017.
  */
object TestHttpParser extends App {


  implicit val fs = LocalFileSystem
  println(IO.fileProducer(Path("d:/marius/dev/idid/shop/shopweb/req.bin")).map {
    case (_, p) =>
      val str = IO.producerToString(p).get
      println(str.substring(1024))
      val r = new HttpParser().parse(str)
      println(r)
      r.map {
        h =>
          println(h.cookies)
      }
  })

}
