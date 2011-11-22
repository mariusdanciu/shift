package net.shift
package demo

import engine.ShiftApplication
import engine.http._
import HttpPredicates._
import netty.NettyServer


object Main extends App {
  println("Starting Netty server")

  def abcService(req: Request):  Option[AsyncResponse => Unit] = Some (
    resp => resp(TextResponse("abc service executed."))
  )

  NettyServer.start(8080, new ShiftApplication {
    def routes: List[Route] = {
      (fullPath("/a/b/c") or POST to abcService) :: Nil
    }
  })


}
