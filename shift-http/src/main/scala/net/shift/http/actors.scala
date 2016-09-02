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
import scala.util.Try
import java.nio.channels.Selector
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.Future
import akka.actor.Terminated

class ServerActor extends Actor with ActorLogging {
  private val actors = new TrieMap[SelectionKey, ActorRef]

  override def postStop() {
    log.info("Cleaning resources.")
    actors.clear()
  }

  def receive = {
    case con @ ClientConnect(socket, _) =>
      val clientActor = context.actorOf(Props[ClientActor])
      clientActor ! con
    case ClientTerminate(key) =>
      actors.get(key) map { act =>
        key.cancel()
        actors -= key
        context.stop(act)
        key.channel().asInstanceOf[SocketChannel].close()
        log.info("Client terminated")
      }
    case ClientStarted(key) =>
      actors.put(key, sender)
      log.info("Accepted new connection from " + key.channel().asInstanceOf[SocketChannel].getRemoteAddress)
  }
}

class ClientActor() extends Actor with ActorLogging {

  var readState: Option[Payload] = None
  var writeState: Option[BinProducer] = None

  var keepAlive: Boolean = false
  var running = false

  val responseIteratee: Iteratee[ByteBuffer, Unit] = Cont {
    case Data(d) => responseIteratee
    case EOF     => Done((), EOF)
  }

  lazy val selector: Selector = Selector.open

  var server: Option[ActorRef] = None

  implicit val ctx = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  override def postStop() {
    log.info("Stopping client handler ... ")
    running = false
    selector.wakeup()
  }

  def receive = {
    case ClientConnect(socket, service) =>
      val key = socket.register(selector, SelectionKey.OP_READ)
      server = Some(sender)
      sender ! ClientStarted(key)
      running = true
      Future { loop(service) } map { _ =>
        ctx.shutdown()
        log.info("Stopped client handler")
      }
  }

  @tailrec
  private def loop(service: HTTPService) {
    if (running) {

      val r = selector.select()

      val keys = selector.selectedKeys().iterator()

      while (keys.hasNext()) {
        val key = keys.next()
        if (!running) {
          key.channel().close()
          key.cancel()
        } else if (key.isValid()) {
          if (key.isReadable()) {
            readChunk(key, service)
          } else if (key.isWritable()) {
            writeResponse(key)
          }
        }
        keys.remove
      }
      loop(service)
    }
  }

  private def terminate(key: SelectionKey) {
    running = false
    selector.wakeup()
    server map { _ ! ClientTerminate(key) }
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
      terminate(key)
    } else {
      unSelectForWrite(key)
      selectForRead(key)
    }
  }

  private def writeResponse(key: SelectionKey) = {
    writeState match {
      case Some(prod) =>
        val client = key.channel().asInstanceOf[SocketChannel]
        send(key, prod) match {
          case Done(_, EOF) =>
            handleResponseSent(key)
          case _ =>
        }

      case _ =>
    }

  }
  private def send(key: SelectionKey, prod: BinProducer): Iteratee[ByteBuffer, Unit] = Try {
    lazy val cont: Iteratee[ByteBuffer, Unit] = Cont {
      case Data(d) =>
        val client = key.channel().asInstanceOf[SocketChannel]
        drain(client, d) match {
          case (0, buf) =>
            writeState = Some(prod)
            selectForWrite(key)
            key.selector().wakeup()
            Done((), Data(d))
          case (_, buf) => cont
        }
      case EOF =>
        selectForRead(key)
        Done((), EOF)
    }
    prod(cont)
  }.recover {
    case e: Exception => net.shift.io.Error[ByteBuffer, Unit](e)
  }.get

  private def readChunk(key: SelectionKey, service: HTTPService) = {

    def checkRequestComplete(bodySize: Long, contentLength: Option[Long], http: HTTPRequest) = {
      if (contentLength.getOrElse(-1L) > bodySize) {
        readState = Some(http)
      } else {
        readState = None
        keepAlive = http.stringHeader("Connection").map { _ == "keep-alive" } getOrElse true
        service(http, resp => {
          send(key, IO.segmentable(resp.asBinProducer)) match {
            case Done(_, EOF) =>
              handleResponseSent(key)
            case _ =>
          }
        })
      }
    }
    Try {
      val client = key.channel().asInstanceOf[SocketChannel]

      val buf = ByteBuffer.allocate(512)
      var size = client.read(buf)

      if (size > 0) {
        buf.flip()
        readState match {
          case RawExtract(raw) =>
            val msg = raw + buf
            tryParse(msg) match {
              case Some(http @ HTTPRequest(m, u, v, hd, body @ HTTPBody(seq))) =>
                checkRequestComplete(body.size, http.longHeader("Content-Length"), http)
              case None =>
                readState = Some(msg)
              case _ =>
                log.error("Cannot read request")
                terminate(key)

            }
          case Some(h @ HTTPRequest(m, u, v, hd, body @ HTTPBody(seq))) =>
            val newSize = body.size + size
            val cl = h.longHeader("Content-Length")
            val msg = HTTPBody(seq ++ Seq(buf))
            val req = HTTPRequest(m, u, v, hd, msg)
            checkRequestComplete(newSize, cl, req)
        }

      } else if (size < 0) {
        log.info("End of client stream")
        terminate(key)
      }
    }.recover {
      case e =>
        log.error("Cannot read data from client: ", e)
        terminate(key)

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