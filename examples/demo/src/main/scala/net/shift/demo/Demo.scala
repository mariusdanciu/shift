package net.shift
package demo

import engine.ShiftApplication
import engine.http._
import HttpPredicates._
import netty.NettyServer


object Main extends App {
  println("Starting Netty server")

  def abcService(req: Request):  Option[AsyncResponse => Unit] = Some (
    resp => { println("abc"); resp(TextResponse("abc service executed."))}
  )

  def yzService(req: Request):  Option[AsyncResponse => Unit] = Some (
    resp => resp(TextResponse("yz service executed."))
  )

  def notFoundService(req: Request):  Option[AsyncResponse => Unit] = Some (
    resp => resp(TextResponse("Sorry ... service not found"))
  )

  def serveService(req: Request):  Option[AsyncResponse => Unit] = Some (
    resp => resp(TextResponse("serve invoked"))
  )


  NettyServer.start(8080, new ShiftApplication {
    def servingRule = (path("a/b/c") or POST then abcService) or
      (tailPath then path("y/z") then yzService) or
      ((matchPath {
        case (r, a :: b :: "serve" :: _) => Some(r)
      }) then serveService) or
      notFoundService
    })


}
