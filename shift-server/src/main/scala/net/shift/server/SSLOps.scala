package net.shift.server

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}
import javax.net.ssl.{SSLEngine, SSLEngineResult, SSLException}

import net.shift.common.LogBuilder

import scala.util.{Failure, Success, Try}

/**
  * Created by mariu on 1/5/2017.
  */
trait SSLOps {
  this: KeyLogger =>

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


  def unwrap(key: SelectionKey,
             engine: SSLEngine,
             clientEncryptedData: ByteBuffer,
             clientDecryptedData: ByteBuffer): OPResult = {

    keyLog(key, "unwrap() clientEncryptedData : " + clientEncryptedData)
    val r = engine.unwrap(clientEncryptedData, clientDecryptedData)

    keyLog(key, "unwrap() result : " + r)

    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        keyLog(key, "unwrap() BUFFER_OVERFLOW " + clientEncryptedData)
        handleBufferOverflow(engine, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        keyLog(key, "unwrap() BUFFER_UNDERFLOW " + clientEncryptedData)
        handleBufferUnderFlow(engine, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.CLOSED =>
        keyLog(key, "unwrap() CLOSED " + clientEncryptedData)
        clientEncryptedData.clear()
        OPResult(SSLEngineResult.Status.CLOSED, clientEncryptedData, clientDecryptedData)

      case SSLEngineResult.Status.OK =>
        keyLog(key, "unwrap() OK " + clientEncryptedData)
        OPResult(SSLEngineResult.Status.OK, clientEncryptedData, clientDecryptedData)
    }
  }

  def wrap(key: SelectionKey,
           engine: SSLEngine,
           serverDecryptedData: ByteBuffer,
           serverEncryptedData: ByteBuffer): OPResult = {

    val r = engine.wrap(serverDecryptedData, serverEncryptedData)

    keyLog(key, "wrap() result : " + r)
    keyLog(key, "serverDecryptedData " + serverDecryptedData)
    keyLog(key, "serverEncryptedData " + serverEncryptedData)


    r.getStatus match {
      case SSLEngineResult.Status.BUFFER_OVERFLOW =>
        keyLog(key, "wrap() BUFFER_OVERFLOW ")

        val netSize = engine.getSession.getPacketBufferSize

        val buf = if (netSize > serverEncryptedData.capacity()) {
          val b = ByteBuffer.allocate(netSize)
          serverEncryptedData.flip()
          b.put(serverEncryptedData)
          b
        } else {
          serverEncryptedData
        }

        wrap(key, engine, buf, serverEncryptedData)

      case SSLEngineResult.Status.BUFFER_UNDERFLOW =>
        keyLog(key, "wrap() BUFFER_UNDERFLOW " + serverEncryptedData)
        OPResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, serverEncryptedData, serverDecryptedData)


      case SSLEngineResult.Status.CLOSED =>
        keyLog(key, "wrap() CLOSED " + serverEncryptedData)
        OPResult(SSLEngineResult.Status.CLOSED, serverEncryptedData, serverDecryptedData)
      case SSLEngineResult.Status.OK =>
        keyLog(key, "wrap() OK " + serverEncryptedData)
        OPResult(SSLEngineResult.Status.OK, serverEncryptedData, serverDecryptedData)
    }
  }

}

case class OPResult(status: SSLEngineResult.Status, sourceBuf: ByteBuffer, destBuff: ByteBuffer)