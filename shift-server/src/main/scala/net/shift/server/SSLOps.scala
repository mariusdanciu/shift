package net.shift.server

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import javax.net.ssl.{SSLEngine, SSLEngineResult, SSLException}

import scala.util.{Failure, Success, Try}

/**
  * Created by mariu on 1/5/2017.
  */
trait SSLOps {
  def unwrap(socket: SocketChannel, engine: SSLEngine, clientEncryptedData: ByteBuffer, clientDecryptedData: ByteBuffer): Try[ByteBuffer] = {
    val r = engine.unwrap(clientEncryptedData, clientDecryptedData)
    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        val appSize = engine.getSession.getApplicationBufferSize
        val b = ByteBuffer.allocate(appSize + clientDecryptedData.position())
        clientDecryptedData.flip()
        b.put(clientDecryptedData)
        unwrap(socket, engine, clientEncryptedData, b)
      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        val netSize = engine.getSession.getPacketBufferSize

        val buf = if (netSize > clientEncryptedData.capacity()) {
          val b = ByteBuffer.allocate(netSize)
          clientEncryptedData.flip()
          b.put(clientEncryptedData)
          b
        } else {
          clientEncryptedData
        }

        unwrap(socket, engine, buf, clientDecryptedData)
      case SSLEngineResult.Status.CLOSED => Failure(new SSLException("Client closed"))
      case SSLEngineResult.Status.OK => Success(clientDecryptedData)
    }
  }

  def wrap(socket: SocketChannel, engine: SSLEngine, serverDecryptedData: ByteBuffer, serverEncryptedData: ByteBuffer): Try[ByteBuffer] = {
    val r = engine.wrap(serverDecryptedData, serverEncryptedData)
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

      case SSLEngineResult.Status.BUFFER_UNDERFLOW  =>
        Failure(new SSLException("Buffer underflow cannot occur on the server data buffer"))

      case SSLEngineResult.Status.CLOSED => Failure(new SSLException("Client closed"))
      case SSLEngineResult.Status.OK => Success(serverEncryptedData)
    }
  }
}
