package net.shift.protocol

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import net.shift.io.BinProducer
import net.shift.server.ResponseContinuationState

trait Protocol {

  def keepConnection: Boolean

  def apply(in: ByteBuffer)(write: (BinProducer, String) => Unit)(implicit ctx: ExecutionContext)

}

