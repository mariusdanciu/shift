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

object HTTPServer {

}

class HTTPServer {

  val state: MMap[SocketChannel, TCPMessage] = MMap.empty

  def start(port: Int) = {

    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)
    serverChannel.bind(new InetSocketAddress(port))

    val selector = Selector.open

    val key = serverChannel.register(selector, serverChannel.validOps(), null)

    while (true) {

      selector.select()

      val keys = selector.selectedKeys().asScala

      for { key <- keys } {
        if (key.isAcceptable()) {
          val client = serverChannel.accept()
          client.configureBlocking(false)
          client.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE)
        } else if (key.isReadable()) {
          val client = key.channel().asInstanceOf[SocketChannel]

          val buf = ByteBuffer.allocate(512)
          var size = client.read(buf)
          val list = new ListBuffer[Array[Byte]]()
          while (size > 0) {
            buf.limit(size)
            val arr = new Array[Byte](size)
            buf.get(arr)
            buf.clear()
            size = client.read(buf)
          }

          if (state.contains(client)) {
            state(client) ++ list.toList
          } else {
            state += (client -> TCPMessage(list.toList))
          }

          continue(state(client)) match {
            case Some(http) =>
              println("read full message")
              println(http)
            case None => println("continue reading")
          }

        } else if (key.isWritable()) {

        }
      }

      //do something with socketChannel...
    }

  }

  private def continue(msg: TCPMessage): Option[HTTP] = BinReader(IO.fromArrays(msg.buffers)).toOption.flatMap { reader =>
    new HttpParser().parse(reader) match {
      case Success(h @ HTTP(_, headers, body)) =>
        println(h)
        for { cl <- h.header("Content-Length") if (cl.value.toInt == body.message.length) } yield {
          h
        }
      case Failure(f) => None
    }
  }

  def stop() = {

  }

}

case class TCPMessage(buffers: List[Array[Byte]]) {
  def +(b: Array[Byte]) = TCPMessage(buffers ++ List(b))
  def ++(b: Seq[Array[Byte]]) = TCPMessage(buffers ++ b)
}

