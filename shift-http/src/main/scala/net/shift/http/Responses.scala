package net.shift
package http

import net.shift.io.IO._
import net.shift.common.Path
import net.shift.io.FileSystem
import net.shift.io.IO
import scala.util.Try
import net.shift.common.FileSplit
import net.shift.io.BinProducer

object Responses {

  def jsFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/javascript")
  def cssFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/css")
  def htmlFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/html")
  def jsonFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/json")
  def xmlFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/xml")
  def textFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "text/plain")
  def binFileResponse(path: Path)(implicit fs: FileSystem) = fileResponse(path, "application/octet-stream")
  def imageFileResponse(path: Path)(implicit fs: FileSystem) = {
    path.parts.last match {
      case FileSplit(name, "svg")  => fileResponse(path, "image/svg+xml")
      case FileSplit(name, "png")  => fileResponse(path, "image/png")
      case FileSplit(name, "jpg")  => fileResponse(path, "image/jpg")
      case FileSplit(name, "jpeg") => fileResponse(path, "image/jpeg")
      case FileSplit(name, "gif")  => fileResponse(path, "image/gif")
      case FileSplit(name, "tif")  => fileResponse(path, "image/tiff")
      case FileSplit(name, "tiff") => fileResponse(path, "image/tiff")
      case FileSplit(name, "bmp")  => fileResponse(path, "image/bmp")
      case FileSplit(name, "ico")  => fileResponse(path, "image/x-icon")

    }

  }

  def fileResponse(path: Path, mime: String)(implicit fs: FileSystem): Try[HTTPResponse] = {
    for { (size, prod) <- IO.fileProducer(path) } yield {
      HTTPResponse(code = 200,
        headers = List(TextHeader("Content-Type", mime),
          TextHeader("Content-Length", size.toString)),
        body = prod)
    }
  }

  def producerResponse(bdy: BinProducer, size: Long) = HTTPResponse(code = 200,
    headers = List(TextHeader("Content-Length", size.toString)),
    body = bdy)

  def textResponse(text: String) = {
    val bd = HTTPBody(text)
    HTTPResponse(code = 200,
      headers = List(
        Headers.contentType(ContentType.TextPlain),
        Headers.contentLength(bd.size)),
      body = bd)
  }

  def ok = HTTPResponse(code = 200, body = HTTPBody.empty)

  def created = HTTPResponse(code = 201, body = HTTPBody.empty)
  def accepted = HTTPResponse(code = 201, body = HTTPBody.empty)

  def redirect(location: String) =
    HTTPResponse(code = 302, headers = List(TextHeader("Location", location)), body = HTTPBody.empty)
    
  def notModified = HTTPResponse(code = 304, body = HTTPBody.empty)

  def badRequest = HTTPResponse(code = 400, body = HTTPBody.empty)

  def basicAuthRequired(msg: String, realm: String) =
    HTTPResponse(code = 401, headers = List(TextHeader("WWW-Authenticate", s"""Basic realm="$realm"""")), body = HTTPBody(msg))

  def paymentRequired = HTTPResponse(code = 402, body = HTTPBody.empty)
  def forbidden = HTTPResponse(code = 403, body = HTTPBody.empty)
  def notFound = HTTPResponse(code = 404, body = HTTPBody.empty)
  def confilct = HTTPResponse(code = 409, body = HTTPBody.empty)

  def serverError = HTTPResponse(code = 500, body = HTTPBody.empty)
  def notImplemented = HTTPResponse(code = 501, body = HTTPBody.empty)
  def serviceUnavailable = HTTPResponse(code = 503, body = HTTPBody.empty)

}
