package net.shift.spray

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.io.IO
import akka.util.Timeout
import akka.util.Timeout.intToTimeout
import net.shift.common.EmptyPath
import net.shift.common.Path
import net.shift.common.StringUtils.toLong
import net.shift.engine.Engine
import net.shift.engine.ShiftApplication
import net.shift.engine.http.Cookie
import net.shift.engine.http.Header
import net.shift.engine.http.HttpUtils
import net.shift.engine.http.Request
import net.shift.engine.http.Response
import net.shift.io.FileSystem
import net.shift.loc.Language
import spray.can.Http
import spray.can.Http.Register
import spray.http.ContentType
import spray.http.ContentTypes
import spray.http.HttpEntity
import spray.http.HttpEntity.apply
import spray.http.HttpHeader
import spray.http.HttpProtocols
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.MediaType
import spray.http.StatusCode.int2StatusCode
import spray.http.Timedout
import spray.http.Rendering
import scala.util.Failure
import com.sun.net.httpserver.HttpServer
import akka.dispatch.sysmsg.Supervise
import akka.actor.TypedActor.Supervisor
import akka.actor.OneForOneStrategy
import akka.actor.Terminated
import spray.http.HttpCookie
import spray.http.StringRendering
import net.shift.common.Config

/**
 * @author marius
 */
object SprayServer {

  implicit val system = ActorSystem()

  def start(port: Int, app: ShiftApplication)(implicit ec: scala.concurrent.ExecutionContext) {
    val server = system.actorOf(Props(new Server(app)), name = "shift-server")

    IO(Http) ! Http.Bind(server, interface = "0.0.0.0", port = port)
  }

}

class Server(app: ShiftApplication) extends Actor with ActorLogging with HttpUtils {
  implicit val timeout: Timeout = Config.int("shift.request.timeout", 20000)
  val Empty = Array[Byte](0)

  def receive = {

    case Http.Connected(remoteAddr, localAddr) =>
      sender ! Register(self)

    case r @ HttpRequest(method, path, headers, entity, protocol) =>

      val client = sender

      Engine.run(app)(toShiftRequest(r), resp => {
        client ! toSprayResponse(resp)
      })

    case Timedout(HttpRequest(method, uri, _, _, _)) =>
      sender ! HttpResponse(
        status = 500,
        entity = Config.string("shift.timeout.message", "The " + method + " request to '" + uri + "' has timed out..."))

    case Terminated(worker) =>
      log.debug(worker + " terminated")
  }

  def toSprayResponse(resp: Response): HttpResponse = {

    val bytes = net.shift.io.IO.toArray(resp.body)

    val body = bytes getOrElse Empty

    val ct = resp.contentType getOrElse "text/plain"

    val cookies = resp.cookies.map { c =>
      {
        val render = new StringRendering
        HttpCookie(c.name, c.value, None, c.maxAge, c.domain, c.path, c.secure, c.httpOnly).render(render)
        Header("Set-Cookie", render.get)
      }
    }

    HttpResponse(status = resp.code,
      entity = HttpEntity(toContentType(ct), body),
      headers = toSprayHeaders(resp.headers ++ cookies ++ List(Header("Content-Length", body.length.toString))),
      protocol = HttpProtocols.`HTTP/1.1`)

  }

  def toSprayHeaders(headers: List[Header]) =
    headers map { h =>
      new HttpHeader() {
        def name = h.key
        def value = h.value
        def lowercaseName = h.key.toLowerCase
        def render[R <: Rendering](r: R): r.type = r ~~ name ~~ ':' ~~ value
      }
    }

  def toContentType(s: String) = {
    s.split("/").toList match {
      case main :: sub :: Nil => ContentType(MediaType.custom(
        mainType = main,
        subType = sub))
      case main :: Nil => ContentType(MediaType.custom(
        mainType = main,
        subType = ""))
      case _ => ContentTypes.`text/plain`
    }
  }

  def toShiftRequest(sprayReq: HttpRequest) = new Request {

    lazy val path = Path(uri)
    def uri = sprayReq.uri.path.toString().substring(1)
    def method = sprayReq.method.value
    def contextPath: Path = EmptyPath

    lazy val queryString = query(sprayReq.uri.toString())

    def param(name: String) = params.get(name)
    lazy val params = qsToParams(queryString) ++ postParams
    
    def header(name: String) = headers.get(name)
    def headers = toHeaders(sprayReq.headers)

    lazy val contentLength = header("Content-Length").map(h => toLong(h.value, 0))
    def contentType = header("Content-Type").map(_.value)
    lazy val cookies = sprayReq.cookies.map { nc =>
      (nc.name, Cookie(nc.name,
        nc.content,
        nc.domain,
        nc.path,
        nc.maxAge,
        Some(1),
        nc.secure,
        nc.httpOnly))
    } toMap

    def cookie(name: String) = cookies.get(name)

    def readBody = net.shift.io.IO.arrayProducer(sprayReq.entity.data.toByteArray)
    def resource(path: Path)(implicit fs: FileSystem) = fs reader path

    lazy val language = Language("en")

    val postParams = (headers.get("Content-Type") match {
      case Some(Header(_, "application/x-www-form-urlencoded", _)) =>
        (net.shift.io.IO.toArray(readBody) map { arr => qsToParams(new String(arr, "UTF-8")) }) getOrElse { Map.empty }
      case _ =>
        Map.empty
    })
  }

  def toHeaders(headers: List[HttpHeader]): Map[String, Header] = {
    ((Map.empty: Map[String, Header]) /: headers) { (a, e) =>
      extractHeaderValue(e.name, e.value) match {
        case Some(header) => a + (header.key -> header)
        case _            => a
      }
    }

  }

  def qsToParams(qs: Option[String]): Map[String, List[String]] = 
    qs map { qsToParams } getOrElse Map.empty

  def qsToParams(qs: String): Map[String, List[String]] = {
    val params = qs.split("&")
    val p = params.toList.map(p => p.split("=").toList match {
      case k :: v :: Nil => (k, v)
      case k :: Nil      => (k, "")
      case _             => ("", "")
    })

    ((Map.empty: Map[String, List[String]]) /: p)((a, e) =>
      if (a.contains(e._1))
        a + ((e._1, (a.get(e._1) getOrElse Nil) ++ List(e._2)))
      else
        a + ((e._1, List(e._2))))

  }

  def query(uri: String): Option[String] = {
    if (uri.isEmpty())
      None
    else {
      val pos = uri.indexOf("?")
      if (pos < 0)
        None
      else
        Some(uri.substring(pos + 1))
    }
  }

}