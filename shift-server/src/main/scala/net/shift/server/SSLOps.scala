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


  def unwrap(socket: SocketChannel,
             engine: SSLEngine,
             clientEncryptedData: ByteBuffer,
             clientDecryptedData: ByteBuffer): OPResult = {

    println("unwrap " + clientEncryptedData)
    println("unwrap " + clientDecryptedData)
    val r = engine.unwrap(clientEncryptedData, clientDecryptedData)
    println(r)
    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        val appSize = engine.getSession.getApplicationBufferSize
        val b = ByteBuffer.allocate(appSize + clientDecryptedData.position())
        clientDecryptedData.flip()
        b.put(clientDecryptedData)
        OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, clientEncryptedData, b)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        val appSize = engine.getSession.getPacketBufferSize
        val b = ByteBuffer.allocate(appSize + clientEncryptedData.position())
        clientEncryptedData.flip()
        b.put(clientEncryptedData)
        b.flip()
        OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, b, clientDecryptedData)

      case SSLEngineResult.Status.CLOSED =>
        clientEncryptedData.clear()
        OPResult(SSLEngineResult.Status.CLOSED, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.OK =>
        println("unwrap written " + clientDecryptedData)
        OPResult(SSLEngineResult.Status.OK, clientEncryptedData, clientDecryptedData)
    }
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
          case e: Throwable =>
            log.error("Failed to send server's CLOSE message due to socket channel's failure.");
        }

        Failure(new SSLException("Client closed"))
      case SSLEngineResult.Status.OK =>
        Success(serverEncryptedData)
    }
  }
}

case class OPResult(status: SSLEngineResult.Status, sourceBuf: ByteBuffer, destBuff: ByteBuffer)