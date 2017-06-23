package net.shift.server.protocol

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import net.shift.io.BinProducer

trait Protocol {

  def keepConnection: Boolean

  def apply(in: ByteBuffer)(write: BinProducer=> Unit)(implicit ctx: ExecutionContext)

}

trait ProtocolBuilder {
  def createProtocol: Protocol
}