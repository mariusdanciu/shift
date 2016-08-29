package net.shift
package http

import scala.collection.concurrent.TrieMap
import akka.actor.Props
import akka.actor.ActorLogging
import akka.actor.PoisonPill
import akka.actor.Actor
import java.nio.channels.SocketChannel
import java.nio.channels.SelectionKey
import akka.actor.ActorRef
import net.shift.io.Iteratee
import net.shift.common.BinReader
import net.shift.io.EOF
import java.nio.ByteBuffer
import net.shift.io.Done
import scala.util.Failure
import net.shift.io.Cont
import net.shift.io.IO
import scala.util.Success
import net.shift.io.Data
import Selections._

class ServerActor extends Actor with ActorLogging {
  private val actors = new TrieMap[SelectionKey, ActorRef]

  def receive = {
    case ClientConnect(key) =>
      val clientActor = context.actorOf(Props[ClientActor])
      actors.put(key, clientActor)
    case ClientTerminate(key) =>
      log.info("Client terminated")
      key.cancel()
      actors -= key
      sender ! PoisonPill
      key.channel().asInstanceOf[SocketChannel].close()

    case r: ReadHttp  => actors.get(r.key) map (_ ! r)
    case w: WriteHttp => actors.get(w.key) map (_ ! w)

  }
}

class ClientActor() extends Actor with ActorLogging {

  var readState: Option[Payload] = None
  var writeState: Option[ByteBuffer] = None

  var keepAlive: Boolean = false

  val responseIteratee: Iteratee[ByteBuffer, Unit] = Cont {
    case Data(d) => responseIteratee
    case EOF     => Done((), EOF)
  }

  def receive = {
    case r: ReadHttp  => readChunk(r)
    case w: WriteHttp => writeResponse(w)
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): ByteBuffer = {
    var written = client.write(buffer)
    while (written > 0 && buffer.hasRemaining()) {
      written = client.write(buffer)
    }
    buffer
  }

  private def handleResponseSent(key: SelectionKey) = {
    writeState = None
    if (!keepAlive) {
      sender ! ClientTerminate(key)
    } else {
      selectForRead(key)

    }
  }

  private def writeResponse(wr: WriteHttp) = {
    writeState match {
      case Some(resp) =>
        val client = wr.key.channel().asInstanceOf[SocketChannel]
        val buf = drain(client, resp)

        if (!resp.hasRemaining()) {
          handleResponseSent(wr.key)
        } else {
          selectForWrite(wr.key)
        }
      case _ =>
    }

  }

  private def readChunk(r: ReadHttp) = {

    def checkRequestComplete(bodySize: Long, contentLength: Option[Long], http: HTTPRequest) = {
      if (contentLength.getOrElse(-1L) > bodySize) {
        readState = Some(http)
      } else {
        readState = None
        keepAlive = http.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
        r.service(http, resp => {
          IO.toBuffer(resp) map { arr =>
            val client = r.key.channel().asInstanceOf[SocketChannel]
            val buf = drain(client, arr)

            if (buf.hasRemaining()) {
              writeState = Some(buf)
              selectForWrite(r.key)
              r.key.selector().wakeup()
            } else {
              handleResponseSent(r.key)
            }
          }
        })
      }
    }

    val client = r.key.channel().asInstanceOf[SocketChannel]

    val buf = ByteBuffer.allocate(512)
    var size = client.read(buf)

    if (size > 0) {
      buf.flip()
      readState match {
        case RawExtract(raw) =>
          val msg = raw + buf
          tryParse(msg) match {
            case Some(http) =>
              checkRequestComplete(http.body.size, http.longHeader("Content-Length"), http)
            case None =>
              readState = Some(msg)
          }
        case Some(h @ HTTPRequest(m, u, v, hd, body)) =>
          val newSize = body.size + size
          val cl = h.longHeader("Content-Length")
          val msg = HTTPBody(body.parts ++ Seq(buf))
          val req = HTTPRequest(m, u, v, hd, msg)
          checkRequestComplete(newSize, cl, req)
      }

    } else if (size < 0) {
      readState = None
      sender ! ClientTerminate(r.key)
    }
  }

  private def tryParse(msg: Raw): Option[HTTPRequest] = {
    BinReader(IO.fromChunks(msg.buffers)).toOption.flatMap { reader =>
      new HttpParser().parse(reader) match {
        case Success(h @ HTTPRequest(_, _, _, headers, body)) =>
          Some(h)
        case Failure(f) =>
          None
      }
    }
  }

}