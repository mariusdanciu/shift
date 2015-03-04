package net.shift

package object io {
  type BinConsumer[O] = Iteratee[Array[Byte], O]
  type BinProducer = IterateeProducer[Array[Byte]]
}