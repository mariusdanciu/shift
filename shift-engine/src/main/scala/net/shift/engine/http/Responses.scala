package net.shift
package engine
package http

import io.WriteChannel
import scala.xml._


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
case class XhtmlResponse(content : NodeSeq) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: WriteChannel) = {
    channel.writeBuffer(new Array[Byte](0))
  }
}
