package net.shift
package template

import common._
import scala.xml.NodeSeq

case class Snippet[T](name: String, f: State[T, NodeSeq]) {
}

trait DynamicContent[T] {
  def snippets: List[Snippet[PageState[T]]]

  def toMap: Map[String, State[PageState[T], NodeSeq]] = snippets.map(s => (s.name, s.f)).toMap
}


