package net.shift
package demo

import scala.xml._
import template._

object FirstPage extends DynamicContent {

  def snippets = List(elem1, elem2)

  val elem1 = Snippet("elem1", {
    case _ => <b/>
  })
  val elem2 = Snippet("elem2", {
    case _ => <b/>
  })
}

