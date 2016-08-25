package net.shift

import java.nio.ByteBuffer
package object io {
  type BinConsumer[O] = Iteratee[ByteBuffer, O]
  type BinProducer = IterateeProducer[ByteBuffer]
}