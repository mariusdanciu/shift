package net.shift.protocol

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import net.shift.io.BinProducer

trait Protocol[REQ] {

  def keepConnection: Boolean

  def apply(in: ByteBuffer)(write: BinProducer => Unit)(implicit ctx: ExecutionContext): Unit

}

