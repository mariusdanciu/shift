package net.shift
package demo

import scala.concurrent.ExecutionContext.global
import common.Config
import engine.ShiftApplication
import engine.ShiftApplication._
import engine.http.Response._
import engine.http._
import net.shift.common.Path
import net.shift.loc.Language
import netty.NettyServer
import template._
import net.shift.engine.page.Html5
import net.shift.engine.utils.ShiftUtils
import net.shift.security.Credentials
import net.shift.security.User
import net.shift.common.ShiftFailure
import scala.util.Success
import net.shift.common.State
import net.shift.common.Base64
import net.shift.security.BasicCredentials
import scala.util.Failure
import net.shift.security.HMac
import net.shift.security.SecurityFailure
import net.shift.security.Users

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

  def serveService(resp: AsyncResponse) {
    resp(TextResponse("serve invoked"))
  }

  Config.load()

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
    } yield service(serveService)

    val r0 = for {
      r <- path("/")
    } yield Html5.pageFromFile(r, r.language, Path("pages/first.html"), FirstPage)

    val multi = for {
      _ <- path("/form")
      r <- req
      mp <- multipartForm
    } yield {
      for { p <- mp.parts } yield {
        p match {
          case b @ BinaryPart(h, content) =>
            for {
              cd <- h.get("Content-Disposition")
              fn <- cd.params.get("filename")
            } yield {
              scalax.file.Path(fn).write(content)
            }
          case t @ TextPart(h, content) => println(t)
        }
      }
      Html5.pageFromFile(r, r.language, Path("pages/first.html"), FirstPage)
    }

    val admin = for {
      _ <- path("/admin")
      user <- authenticate
    } yield {
      println("Got user " + user)
      service(_(TextResponse("admin").withSecurityCookies(user)))
    }

    implicit def login(creds: Credentials): Option[User] = {
      creds match {
        case BasicCredentials("marius", "boot") => Some(User("marius", None, Set.empty))
        case _                                  => None
      }
    }

    def servingRule = staticFiles(Path("web")) |
      r0 |
      r1 |
      r2 |
      r3 |
      multi |
      admin |
      r4 |
      service(notFoundService)
  })

}
