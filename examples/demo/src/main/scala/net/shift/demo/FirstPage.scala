package net.shift
package demo

import scala.xml._
import template._
import engine.http._
import Snippet._

object FirstPage extends DynamicContent[Request] {

  def snippets = List(elem1, elem2)
 
  def reqSnip(name: String) = snip[Request](name) _
  
  val elem1 = reqSnip("elem1"){
    s => (s.req, <p>Elem1</p>)
  }
  
  val elem2 = snipNoState[Request]("elem2"){
    s => <p>Elem2</p>
  }
}

