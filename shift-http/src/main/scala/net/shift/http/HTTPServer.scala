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

object HTTPServer extends App {

  import scala.concurrent.ExecutionContext.Implicits.global
  new HTTPServer().start(8080)
}

class HTTPServer {

  def start(port: Int)(implicit ctx: ExecutionContext) = {

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

        if (key.isAcceptable()) {
          val client = serverChannel.accept()
          if (client != null) {
            client.configureBlocking(false)
            client.register(selector, SelectionKey.OP_READ)
          }
        } else if (key.isReadable()) {
          readChunk(key)
        } else if (key.isWritable()) {
          println("Write")
        }

        keys.remove()
      }

      //do something with socketChannel...
    }

  }

  private def readChunk(key: SelectionKey)(implicit ctx: ExecutionContext) = {
    val client = key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(100)
    var size = client.read(buf)
    if (size > 0) {
      buf.flip()
      val arr = new Array[Byte](size)
      buf.get(arr)
      buf.clear()

      val attach = key.attachment()

      attach match {
        case RawExtract(bufs) =>
          val msgs = bufs ++ List(arr)

          tryParse(msgs) match {
            case Some(http) =>
              val sz = http.body.size
              val cl = http.contentLength
              if (cl > sz) {
                key.attach(http)
              } else {
                println("END " + http)
                key.attach(null)
              }
            case None =>
              key.attach(msgs)
              println("continue reading ")
          }
        case h @ HTTPRequest(m, u, v, hd, body) =>
          val newSize = body.size + size
          val cl = h.contentLength
          val msg = HTTPBody(body.parts ++ Seq(arr))
          val req = HTTPRequest(m, u, v, hd, msg)
          if (cl > newSize) {
            println(msg.size)
            key.attach(req)
          } else {
            println(h)
            println(new String(req.body.compact, "UTF-8"))
            key.attach(null)
          }

      }

    } else if (size < 0) {
      key.attach(null)
      key.cancel()
      client.close()
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

  def stop() = {

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

