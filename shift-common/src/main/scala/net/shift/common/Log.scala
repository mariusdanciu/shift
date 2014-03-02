package net.shift.common

import org.apache.log4j.Logger
import org.apache.log4j.Level

trait Log {
  val log = Logger.getLogger(getClass())

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