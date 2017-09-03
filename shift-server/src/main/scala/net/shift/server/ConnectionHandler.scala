package net.shift.server

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}

import net.shift.io._
import net.shift.server.Selections.selectForWrite
import net.shift.server.protocol.Protocol

import scala.concurrent.ExecutionContext
import scala.util.Try
import Selections._

/**
  * Created by mariu on 6/23/2017.
  */
trait ConnectionHandler {
  this: KeyLogger =>

  def handleRead()(implicit ec: ExecutionContext): Unit

  def handleWrite()(implicit ec: ExecutionContext): Unit

  def start(): Unit = ()

  def terminate(): Unit


  protected var writeState: Option[ResponseContinuationState] = None
  protected val key: SelectionKey
  protected val protocol: Protocol

  def continueSending(drain: (SocketChannel, ByteBuffer) => (Int, ByteBuffer)): Unit = {
    writeState foreach { st =>
      try {

        lazy val cont: Iteratee[ByteBuffer, Option[ResponseContinuationState]] = Cont {
          case Data(d) =>
            keyLog(key, "Sending buffer " + System.identityHashCode(d) + " " + d)
            val client = key.channel().asInstanceOf[SocketChannel]
            drain(client, d) match {
              case (0, _) =>
                keyLog(key, "Socket full " + System.identityHashCode(d))
                Done(writeState, Empty)
              case (-1, _) =>
                net.shift.io.Error[ByteBuffer, Option[ResponseContinuationState]](new IOException("Client connection closed."))
              case (_, _) => cont
            }
          case EOF =>
            Done(None, EOF)
        }

        st.content(cont) match {
          case Done(s, Empty) =>
            keyLog(key, "Response: continue sending")
            writeState = s
            selectForWrite(key)
            key.selector().wakeup()
          case Done(_, EOF) =>
            keyLog(key, "Done sending response")
            handleResponseSent()
          case Error(t) =>
            log.error("Cannot sent response ", t)
            terminate()
          case it =>
            log.error("Unexpected iteratee " + it)
            terminate()
        }

      } catch {
        case e: Throwable =>
          log.error("Error during writing", e)
          terminate()
          throw e
      }
    }
  }

  private def handleResponseSent() {
    if (!protocol.keepConnection) {
      terminate()
    } else {
      unSelectForWrite(key)
    }
  }

}
