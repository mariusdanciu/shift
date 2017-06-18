package net.shift.server.http

object ContentType {
  val TextPlain = "text/plain; charset=utf-8"
  val TextHtml = "text/html; charset=utf-8"
  val TextXml = "text/xml; charset=utf-8"
  val TextJson = "text/json; charset=utf-8"
  val TextJavascript = "text/javascript; charset=utf-8"
  val TextCss = "text/css; charset=utf-8"
  val Bin = "application/octet-stream"

  val ImageSvg = "image/svg+xml"
  val ImagePng = "image/png"
  val ImageJpg = "image/jpg"
  val ImageJpeg = "image/jpeg"
  val ImageGif = "image/gif"
  val ImageTiff = "image/tiff"
  val ImageBmp = "image/bmp"
  val ImageXIcon = "image/x-icon"

  val FontWoff = "application/font-woff"
  val FontTtf = "application/font-ttf"
  val FontEot = "application/vnd.ms-fontobject"
  val FontOtf = "application/font-otf"

}

object Headers {
  def contentLength(len: Int) = TextHeader("Content-Length", len.toString)
  def contentType(mime: String) = TextHeader("Content-Type", mime)
}

object ExtentionToMime {
  val map = Map(
    "txt" -> ContentType.TextPlain,
    "html" -> ContentType.TextHtml,
    "xml" -> ContentType.TextXml,
    "json" -> ContentType.TextJson,
    "js" -> ContentType.TextJavascript,
    "css" -> ContentType.TextCss,
    "bin" -> ContentType.Bin,

    "svg" -> ContentType.ImageSvg,
    "png" -> ContentType.ImagePng,
    "jpg" -> ContentType.ImageJpg,
    "jpeg" -> ContentType.ImageJpeg,
    "gif" -> ContentType.ImageGif,
    "tif" -> ContentType.ImageTiff,
    "bmp" -> ContentType.ImageBmp,
    "ico" -> ContentType.ImageXIcon,

    "woff" -> ContentType.FontWoff,
    "ttf" -> ContentType.FontTtf,
    "eot" -> ContentType.FontEot,
    "otf" -> ContentType.FontOtf)
}