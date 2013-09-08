package net.shift
package engine
package http

import io.WriteChannel
import scala.xml._
import common._

case class TextResponse(text: String) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/plain; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: WriteChannel) = {
    channel.writeBuffer(text getBytes ("UTF-8"))
  }
}

case class Html5Response(content: NodeSeq) extends Response {
  def code = 200
  def reason = "OK"
  def headers = Map.empty
  def contentType = Some("text/html; charset=\"UTF-8\"")
  def cookies = Nil
  def writeBody(channel: WriteChannel) = {
    channel.writeBuffer(("<!DOCTYPE html>\n" + XmlUtils.mkString(content)).getBytes("UTF-8"))
  }
}

 case class CSSResponse(content: String) extends Response {
   def code = 200
   def reason = "OK"
   def headers = Map.empty
   def contentType = Some("text/css; charset=\"UTF-8\"")
   def cookies = Nil
   def writeBody(channel: WriteChannel) = {
     channel.writeBuffer(content.getBytes("UTF-8"))
   }
 }

 case class JSResponse(content: String) extends Response {
   def code = 200
   def reason = "OK"
   def headers = Map.empty
   def contentType = Some("text/javascript; charset=\"UTF-8\"")
   def cookies = Nil
   def writeBody(channel: WriteChannel) = {
     channel.writeBuffer(content.getBytes("UTF-8"))
   }
 }
