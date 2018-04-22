package net.shift.common

import org.apache.log4j.{Level, Logger}

trait Log {

  def loggerName: String

  protected val log = Logger.getLogger(loggerName)

  def isOff = log.isEnabledFor(Level.OFF)
  def isAll = log.isEnabledFor(Level.ALL)
  def isDebug = log.isEnabledFor(Level.DEBUG)
  def isInfo = log.isEnabledFor(Level.INFO)
  def isWarn = log.isEnabledFor(Level.WARN)
  def isError = log.isEnabledFor(Level.ERROR)

  def debug(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.DEBUG)) {
      if (e != null)
        log.debug(s, e)
      else
        log.debug(s)
    }
  }

  def info(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.INFO)) {
      if (e != null)
        log.info(s, e)
      else
        log.info(s)
    }
  }

  def warn(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.WARN)) {
      if (e != null)
        log.warn(s, e)
      else
        log.warn(s)
    }
  }

  def error(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.ERROR)) {
      if (e != null)
        log.error(s, e)
      else
        log.error(s)
    }
  }

  def fatal(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.FATAL)) {
      if (e != null)
        log.fatal(s, e)
      else
        log.fatal(s)
    }
  }

  def trace(s: => String, e: Throwable = null) {
    if (log.isEnabledFor(Level.TRACE)) {
      if (e != null)
        log.trace(s, e)
      else
        log.trace(s)
    }
  }

  def log(l: Level, s: => String, e: Throwable = null) {
    if (log.isEnabledFor(l)) {
      if (e != null)
        log.log(l, s, e)
      else
        log.log(l, s)
    }
  }

}

trait DefaultLog extends Log {
  def loggerName = getClass().getName()
}

object LogBuilder {
  def logger(name: String) : Log = new Log {
    def loggerName = name
  }
  def logger(cls: Class[_]) : Log = new Log {
    def loggerName = cls.getName
  }
}