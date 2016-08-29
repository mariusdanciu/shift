package net.shift.http

import java.nio.channels.SelectionKey

object Selections {

  def selectForRead(key: SelectionKey): SelectionKey = {
    key.interestOps(key.interestOps() & SelectionKey.OP_READ)
  }

  def unSelectForRead(key: SelectionKey): SelectionKey = {
    key.interestOps(key.interestOps() & ~SelectionKey.OP_READ)
  }

  def selectForWrite(key: SelectionKey): SelectionKey = {
    key.interestOps(key.interestOps() & SelectionKey.OP_WRITE)
  }

  def unSelectForWrite(key: SelectionKey): SelectionKey = {
    key.interestOps(key.interestOps() & ~SelectionKey.OP_WRITE)
  }
}