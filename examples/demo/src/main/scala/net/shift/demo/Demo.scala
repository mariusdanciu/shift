package net.shift
package demo

import scala.concurrent.ExecutionContext.global
import common.Config
import engine.ShiftApplication
import engine.ShiftApplication._
import engine.http._
import net.shift.common.Path
import net.shift.loc.Language
import netty.NettyServer
import template._
import net.shift.engine.page.Html5
import net.shift.engine.utils.ShiftUtils

object Main extends App with HttpPredicates with ShiftUtils {
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

  Config.load()
  println(Config.string("domain"))

  import scala.concurrent.ExecutionContext.Implicits.global

  NettyServer.start(8080, new ShiftApplication with Selectors {

    implicit val selector = bySnippetAttr[SnipState[Request]]

    import Request._

    // If we have a GET request and the path is /a/b/c
    val r1 = for {
      _ <- GET
      _ <- path("/a/b/c")
    } yield service(abcService)

    // Serve /page/first page
    val r2 = for {
      _ <- path("/page/first")
      r <- withLanguage(Language("ro"))
    } yield {
      Html5.pageFromFile(r, r.language, Path("pages/first.html"), FirstPage)
    }

    // Serve /?/y/z where first part can be anything
    val r3 = for {
      _ <- tailPath
      _ <- path("/y/z")
    } yield service(yzService)

    // Serve ?/1/?/?/3 the first and the two parts in the middle can be anything
    val r4 = for {
      Path("1" :: a :: b :: "3" :: Nil) <- tailPath
      r <- req
    } yield service(serveService(r))

    val r0 = for {
      r <- path("/")
    } yield Html5.pageFromFile(r, r.language, Path("pages/first.html"), FirstPage)

    def servingRule = r0 |
      r1 |
      r2 |
      r3 |
      r4 |
      staticFiles(Path("web")) |
      service(notFoundService)
  })

}
