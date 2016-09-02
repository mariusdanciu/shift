package net.shift.http

import java.nio.channels.SocketChannel
import java.nio.channels.SelectionKey

trait ServerMessage

case class ClientTerminate(key: SelectionKey) extends ServerMessage
case class ClientConnect(socket: SocketChannel, service: HTTPService) extends ServerMessage
case class ClientStarted(key: SelectionKey) extends ServerMessage
