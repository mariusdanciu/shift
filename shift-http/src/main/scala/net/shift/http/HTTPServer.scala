package net.shift.http

import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.Selector

import scala.collection.JavaConverters._
import java.nio.channels.SelectionKey
import java.nio.channels.SocketChannel
import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer

object HTTPServer {

}

class HTTPServer {

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
          
          val array = new Array[Byte](512)
          val buf = ByteBuffer.wrap(array)
          var size = client.read(buf)
          val list = new ListBuffer[Array[Byte]]()
          while (size > 0) {
            list += array.slice(0, size)
            size = client.read(buf)
          }

        } else if (key.isWritable()) {

        }
      }

      //do something with socketChannel...
    }

  }

  def stop() = {

  }

}