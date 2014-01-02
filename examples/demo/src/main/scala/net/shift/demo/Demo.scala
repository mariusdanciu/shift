package net.shift
package demo

import engine.ShiftApplication
import engine.http._
import engine.page._
import HttpPredicates._
import template._
import netty.NettyServer
import ShiftApplication._
import net.shift.common.Path

object Main extends App {
  println("Starting Netty server")

  def abcService(resp: AsyncResponse) {
    resp(TextResponse("abc service executed."))
  }

  def yzService(resp: AsyncResponse) {
    resp(TextResponse("yz service executed."))
  }
  def notFoundService(resp: AsyncResponse) {
    resp(TextResponse("Sorry ... service not found"))
  }

  def serveService(req: Request)(resp: AsyncResponse) {
    resp(TextResponse("serve invoked"))
  }

  NettyServer.start(8080, new ShiftApplication {

    implicit val selector = Selectors.bySnippetAttr[SnipState[Request]]

    // If we have a GET request and the path is /a/b/c
    val r1 = for {
      _ <- GET
      _ <- path("/a/b/c")
    } yield service(abcService)

    // Serve /page/first page
    val r2 = for {
      _ <- path("/page/first")
    } yield {
      Html5(Path("pages/first.html"), FirstPage)
    }

    // Serve /?/y/z where first part can be anything
    val r3 = for {
      _ <- tailPath
      _ <- path("/y/z")
    } yield service(yzService)

    // Serve ?/1/?/?/3 the first and the two parts in the middle can be anything
    val r4 = for {
      Path("1" :: a :: b :: "3" :: Nil) <- tailPath
    } yield serviceWithRequest(serveService)

    val r0 = for {
      _ <- path("/")
    } yield Html5(Path("pages/first.html"), FirstPage)

    def servingRule = r0 |
      r1 |
      r2 |
      r3 |
      r4 |
      service(notFoundService)
  })

}
