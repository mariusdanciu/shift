package net.shift {
package template {

import net.shift.http._
import net.shift.util._

import scala.xml._


object XhtmlTemplateEngine {
  def apply(node : NodeSeq) = new XhtmlTemplateEngine(node)
}

trait TemplateEngine[+A <: NodeSeq] extends NodeSeq {

  def unit[B >: A <: NodeSeq](a: B): TemplateEngine[B]

  def mult[B >: A <: NodeSeq](a: TemplateEngine[TemplateEngine[B]]): TemplateEngine[B]

  def map[B <: NodeSeq](f: A => B): TemplateEngine[B]

  def flatMap[B <: NodeSeq](f: A => TemplateEngine[B]): TemplateEngine[B]

  def filter(f: A => Boolean): TemplateEngine[A]

}

class XhtmlTemplateEngine[+A <: NodeSeq](val xhtml: A) extends TemplateEngine[A] {

  def unit[B <: NodeSeq](a: B): TemplateEngine[B] = new XhtmlTemplateEngine(a)

  def mult[B >: A <: NodeSeq](a: TemplateEngine[TemplateEngine[B]]): TemplateEngine[B] = a.flatMap(n => n)

  def map[B <: NodeSeq](f: A => B): TemplateEngine[B] = unit(f(run))

  def flatMap[B <: NodeSeq](f: A => TemplateEngine[B]): TemplateEngine[B] = f(run)

  def filter(f: A => Boolean): TemplateEngine[A] = if (f(run)) this else NoTemplate

  def theSeq = xhtml.theSeq

  protected def run: A = {
    xhtml
  }

}

object NoTemplate extends TemplateEngine[Nothing] {
  type A = Nothing

  def unit[B >: A <: NodeSeq](a: B): TemplateEngine[B] = NoTemplate

  def mult[B >: A <: NodeSeq](a: TemplateEngine[TemplateEngine[B]]): TemplateEngine[B] = NoTemplate

  def map[B <: NodeSeq](f: A => B): TemplateEngine[B] = NoTemplate

  def flatMap[B <: NodeSeq](f: A => TemplateEngine[B]): TemplateEngine[B] = NoTemplate

  def filter(f: A => Boolean): TemplateEngine[A] = NoTemplate

  def theSeq = NodeSeq.Empty
}


}
}
