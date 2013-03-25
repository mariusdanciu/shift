package net.shift
package template

import scala.xml.NodeSeq

case class Snippet(name: String, f: SnippetFunc) {
}

trait DynamicContent {
  def snippets: List[Snippet]

  def toMap: Map[String, SnippetFunc] = snippets.map(s => (s.name, s.f)).toMap
}


