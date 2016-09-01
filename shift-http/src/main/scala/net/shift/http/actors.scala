package net.shift
package http

import java.nio.ByteBuffer
import java.nio.channels.SelectionKey
import java.nio.channels.SocketChannel
import scala.collection.concurrent.TrieMap
import scala.util.Failure
import scala.util.Success
import Selections._
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props
import net.shift.common.BinReader
import net.shift.io.Cont
import net.shift.io.Data
import net.shift.io.Done
import net.shift.io.EOF
import net.shift.io.IO
import net.shift.io.In
import net.shift.io.Iteratee
import java.io.IOException
import net.shift.io.BinProducer

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
  var writeState: Option[BinProducer] = None

  var keepAlive: Boolean = false

  val responseIteratee: Iteratee[ByteBuffer, Unit] = Cont {
    case Data(d) => responseIteratee
    case EOF     => Done((), EOF)
  }

  def receive = {
    case r: ReadHttp  => readChunk(r)
    case w: WriteHttp => writeResponse(w)
  }

  private def drain(client: SocketChannel, buffer: ByteBuffer): (Int, ByteBuffer) = {
    var written = client.write(buffer)
    while (written > 0 && buffer.hasRemaining()) {
      written = client.write(buffer)
    }
    (written, buffer)
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
      case Some(prod) =>
        val client = wr.key.channel().asInstanceOf[SocketChannel]
        send(wr.key, prod) match {
          case Done(_, EOF) =>
            handleResponseSent(wr.key)
          case _ => selectForWrite(wr.key)
        }

      case _ =>
    }

  }

  private def send(key: SelectionKey, prod: BinProducer): Iteratee[ByteBuffer, Unit] = {
    lazy val cont: Iteratee[ByteBuffer, Unit] = Cont {
      case Data(d) =>
        val client = key.channel().asInstanceOf[SocketChannel]
        drain(client, d) match {
          case (0, buf) =>
            writeState = Some(prod)
            selectForWrite(key)
            key.selector().wakeup()
            Done((), Data(d))
          case (-1, _) =>
            sender ! ClientTerminate(key)
            net.shift.io.Error(new IOException("Cannot write response"))
          case (_, buf) => cont
        }
      case EOF =>
        selectForRead(key)
        Done((), EOF)
    }
    prod(cont)
  }
  private def readChunk(r: ReadHttp) = {

    def checkRequestComplete(bodySize: Long, contentLength: Option[Long], http: HTTPRequest) = {
      if (contentLength.getOrElse(-1L) > bodySize) {
        readState = Some(http)
      } else {
        readState = None
        keepAlive = http.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
        r.service(http, resp => {
          send(r.key, IO.segmentable(resp.asBinProducer)) match {
            case Done(_, EOF) =>
              handleResponseSent(r.key)
            case _ =>
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