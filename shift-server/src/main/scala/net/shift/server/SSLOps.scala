package net.shift.server

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import javax.net.ssl.{SSLEngine, SSLEngineResult, SSLException}

import net.shift.common.LogBuilder

import scala.util.{Failure, Success, Try}

/**
  * Created by mariu on 1/5/2017.
  */
trait SSLOps {

  protected val log = LogBuilder.logger(classOf[SSLServer])


  def unwrap(socket: SocketChannel, engine: SSLEngine, clientEncryptedData: ByteBuffer, clientDecryptedData: ByteBuffer): Try[ByteBuffer] = {
???
  }

  def wrap(socket: SocketChannel, engine: SSLEngine, serverDecryptedData: ByteBuffer, serverEncryptedData: ByteBuffer): Try[ByteBuffer] = {
    val r = engine.wrap(serverDecryptedData, serverEncryptedData)
    println(r)

    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        val netSize = engine.getSession.getPacketBufferSize

        val buf = if (netSize > serverEncryptedData.capacity()) {
          val b = ByteBuffer.allocate(netSize)
          serverEncryptedData.flip()
          b.put(serverEncryptedData)
          b
        } else {
          serverEncryptedData
        }

        wrap(socket, engine, buf, serverEncryptedData)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        Failure(new SSLException("Buffer underflow cannot occur on the server data buffer"))

      case SSLEngineResult.Status.CLOSED =>
        try {
          serverEncryptedData.flip()
          while (serverEncryptedData.hasRemaining()) {
            socket.write(serverEncryptedData)
          }
        } catch {
          case e =>
            log.error("Failed to send server's CLOSE message due to socket channel's failure.");
        }

        Failure(new SSLException("Client closed"))
      case SSLEngineResult.Status.OK =>
        Success(serverEncryptedData)
    }
  }
}
