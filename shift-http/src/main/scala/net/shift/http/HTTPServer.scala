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

object HTTPServer extends App {

  new HTTPServer().start(8080)
}

class HTTPServer {

  def start(port: Int) = {

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

  private def readChunk(key: SelectionKey) = {
    val client = key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)
    if (size > 0) {
      buf.flip()
      val arr = new Array[Byte](size)
      buf.get(arr)
      buf.clear()

      val currentMsgs = key.attachment().asInstanceOf[TCPMessage]

      val msgs = if (currentMsgs != null) {
        currentMsgs ++ List(arr)
      } else {
        TCPMessage(List(arr))
      }

      key.attach(msgs)

      continue(msgs) match {
        case Some(http) =>
          println(http)
          println(new String(http.body.message, "UTF-8"))
          key.attach(null)
          key.cancel()
        case None =>
          println("continue reading")
      }

    } else if (size < 0) {
      key.attach(null)
      key.cancel()
      client.close()
    }
  }

  private def continue(msg: TCPMessage): Option[HTTPRequest] = {
    BinReader(IO.fromArrays(msg.buffers)).toOption.flatMap { reader =>
      new HttpParser().parse(reader) match {
        case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
          for { cl <- h.header("Content-Length") if (cl.value.trim.toInt == body.message.length) } yield {
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

case class TCPMessage(buffers: List[Array[Byte]]) {
  def +(b: Array[Byte]) = TCPMessage(buffers ++ List(b))
  def ++(b: Seq[Array[Byte]]) = TCPMessage(buffers ++ b)
}

