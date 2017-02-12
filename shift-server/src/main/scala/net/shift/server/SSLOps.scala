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

  def handleBufferOverflow(engine: SSLEngine,
                           clientEncryptedData: ByteBuffer,
                           clientDecryptedData: ByteBuffer): OPResult = {
    val appSize = engine.getSession.getApplicationBufferSize
    val b = ByteBuffer.allocate(appSize + clientDecryptedData.position())
    clientDecryptedData.flip()
    b.put(clientDecryptedData)
    OPResult(SSLEngineResult.Status.BUFFER_OVERFLOW, clientEncryptedData, b)
  }

  def handleBufferUnderFlow(engine: SSLEngine,
                            clientEncryptedData: ByteBuffer,
                            clientDecryptedData: ByteBuffer): OPResult = {
    val appSize = engine.getSession.getPacketBufferSize
    val b = ByteBuffer.allocate(appSize + clientEncryptedData.position())
    clientEncryptedData.flip()
    b.put(clientEncryptedData)
    b.flip()
    OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, b, clientDecryptedData)
  }


  def unwrap(engine: SSLEngine,
             clientEncryptedData: ByteBuffer,
             clientDecryptedData: ByteBuffer): OPResult = {

    log.debug("unwrap enc" + clientEncryptedData + " dec " + clientDecryptedData)
    val r = this.synchronized {
      engine.unwrap(clientEncryptedData, clientDecryptedData)
    }
    log.debug("Result " + r)

    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        handleBufferOverflow(engine, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        handleBufferUnderFlow(engine, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.CLOSED =>
        clientEncryptedData.clear()
        OPResult(SSLEngineResult.Status.CLOSED, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.OK =>
        log.debug("unwrap written " + clientDecryptedData)
        OPResult(SSLEngineResult.Status.OK, clientEncryptedData, clientDecryptedData)
    }
  }

  def wrap(engine: SSLEngine, serverDecryptedData: ByteBuffer, serverEncryptedData: ByteBuffer): Try[ByteBuffer] = {
    val r = this.synchronized {
      engine.wrap(serverDecryptedData, serverEncryptedData)
    }
    log.debug("Result " + r)

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

        wrap(engine, buf, serverEncryptedData)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        Failure(new SSLException("Buffer underflow cannot occur on the server data buffer"))

      case SSLEngineResult.Status.CLOSED =>
        Success(serverEncryptedData)
      case SSLEngineResult.Status.OK =>
        Success(serverEncryptedData)
    }
  }
}

case class OPResult(status: SSLEngineResult.Status, sourceBuf: ByteBuffer, destBuff: ByteBuffer)