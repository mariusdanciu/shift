package net.shift.server

import java.nio.channels.SelectionKey

import net.shift.common.Log

/**
  * Created by mariu on 6/21/2017.
  */
trait KeyLogger {

  protected val log: Log

  def keyLog(key: SelectionKey, str: => String): Unit = {
    log.debug(keyToString(key) + str)
  }

  def keyError(key: SelectionKey, str: => String): Unit = {
    log.error(keyToString(key) + str)
  }


  private def keyToString(key: SelectionKey) = s"Key $key: r:${key.isReadable}, w:${key.isWritable}, a:${key.isAcceptable} - "


}
