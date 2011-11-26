package net.shift
package engine
package http

import io.WriteChannel

case class TextResponse(text: String) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: WriteChannel) = {
    channel.writeBuffer(text getBytes("UTF-8"))
  }
}
