package net.shift.http

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.nio.channels.SocketChannel
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import net.shift.common.BinReader
import net.shift.io.IO
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import akka.actor.Actor
import scala.collection.concurrent.TrieMap
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.PoisonPill
import java.net.SocketOption
import java.net.StandardSocketOptions

object Test extends App {
  def serve: HTTPService = {
    case (req, f) =>
      println(req)
      println(IO.toString(req.body))
      f(Responses.text("Some response"))
  }

  val srv = new HTTPServer()
  srv.start(serve, 8080)

}

object HTTPServer {

  protected[http] val system = ActorSystem("HTTPServer")

}

class HTTPServer() {

  val actors: TrieMap[SelectionKey, ActorRef] = new TrieMap[SelectionKey, ActorRef]

  def start(service: HTTPService, port: Int) = {

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    serverChannel.bind(new InetSocketAddress(port))

    val selector = Selector.open

    val key = serverChannel.register(selector, SelectionKey.OP_ACCEPT, null)

    while (true) {

      val r = selector.select()

      val keys = selector.selectedKeys().iterator()

      while (keys.hasNext()) {
        val key = keys.next()
        if (key.isValid()) {
          if (key.isAcceptable()) {
            val client = serverChannel.accept()
            if (client != null) {
              client.configureBlocking(false)
              val clientKey = client.register(selector, SelectionKey.OP_READ)
              actors.put(clientKey, HTTPServer.system.actorOf(Props[ClientActor]))
            }
          } else if (key.isReadable()) {
            actors(key) ! Read(key, service)
          } else if (key.isWritable()) {
            actors(key) ! Write(key)
          }
        }
        keys.remove()
      }

      //do something with socketChannel...
    }

  }

  def stop() = {

  }

}

trait ServerMessage

case class Read(key: SelectionKey, service: HTTPService) extends ServerMessage
case class Write(key: SelectionKey) extends ServerMessage

class ClientActor extends Actor {

  def receive = {
    case Read(key, service) => readChunk(key, service)
    case Write(key)         => writeResponse(key)

  }

  private def writeResponse(key: SelectionKey) = {
    val resp = key.attachment().asInstanceOf[ByteBuffer]
    if (resp != null) {
      val client = key.channel().asInstanceOf[SocketChannel]
      val written = client.write(resp)
      if (!resp.hasRemaining()) {
        key.attach(null)
        //key.interestOps(SelectionKey.OP_READ)
        key.cancel()
        client.close()
        context.stop(self)
      }
    }
  }

  private def readChunk(key: SelectionKey, service: HTTPService) = {
    val client = key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)
    if (size > 0) {
      buf.flip()
      val arr = new Array[Byte](size)
      buf.get(arr)
      buf.clear()

      key.attachment() match {
        case RawExtract(bufs) =>
          val msgs = bufs ++ List(arr)

          tryParse(msgs) match {
            case Some(http) =>
              val sz = http.body.size
              val cl = http.contentLength
              if (cl > sz) {
                key.attach(http)
              } else {
                key.attach(null)
                service(http, resp => {
                  IO.toArray(resp) map { arr =>
                    key.attach(ByteBuffer.wrap(arr))
                    key.interestOps(SelectionKey.OP_WRITE)
                    key.selector().wakeup()
                  }
                })
              }
            case None =>
              key.attach(msgs)
          }
        case h @ HTTPRequest(m, u, v, hd, body) =>
          val newSize = body.size + size
          val cl = h.contentLength
          val msg = HTTPBody(body.parts ++ Seq(arr))
          val req = HTTPRequest(m, u, v, hd, msg)
          if (cl > newSize) {
            key.attach(req)
          } else {
            key.attach(null)
            service(req, resp => {
              IO.toArray(resp) map { arr =>
                key.attach(ByteBuffer.wrap(arr))
                key.interestOps(SelectionKey.OP_WRITE)
              }
            })
          }

      }

    } else if (size < 0) {
      key.attach(null)
      key.cancel()
      client.close()
      context.stop(self)
    }
  }

  private def tryParse(msg: Raw): Option[HTTPRequest] = {
    BinReader(IO.fromArrays(msg.buffers)).toOption.flatMap { reader =>
      new HttpParser().parse(reader) match {
        case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
          for { cl <- h.header("Content-Length") } yield {
            h
          }
        case Failure(f) =>
          None
      }
    }
  }

}

object RawExtract {

  def unapply(t: Any): Option[Raw] =
    if (t == null)
      Some(Raw(Nil))
    else {
      t match {
        case r: Raw => Some(Raw(r.buffers))
        case _      => None
      }
    }
}

case class Raw(buffers: List[Array[Byte]]) {
  def +(b: Array[Byte]) = Raw(buffers ++ List(b))
  def ++(b: Seq[Array[Byte]]) = Raw(buffers ++ b)
}

