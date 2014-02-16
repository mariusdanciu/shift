package net.shift.common

import org.apache.log4j.Logger
import org.apache.log4j.Level

trait Log {
  val log = Logger.getLogger(getClass())

  def debug(s: => String) {
    if (log.isEnabledFor(Level.DEBUG)) {
      log debug s
    }
  }

  def info(s: => String) {
    if (log.isEnabledFor(Level.INFO)) {
      log info s
    }
  }

  def warn(s: => String) {
    if (log.isEnabledFor(Level.WARN)) {
      log warn s
    }
  }

  def error(s: => String) {
    if (log.isEnabledFor(Level.ERROR)) {
      log error s
    }
  }

  def fatal(s: => String) {
    if (log.isEnabledFor(Level.FATAL)) {
      log fatal s
    }
  }

  def trace(s: => String) {
    if (log.isEnabledFor(Level.TRACE)) {
      log trace s
    }
  }

  def entry(l: Level, s: => String) {
    if (log.isEnabledFor(l)) {
      log.log(l, s)
    }
  }
}