package net.shift
package demo

import scala.xml._
import template._
import engine.http._
import Snippet._
import loc.Loc._
import java.util.Locale

object FirstPage extends DynamicContent[Request] {

  val ? = loc("test", new Locale("ro")) _
  
  def snippets = List(elem1, elem2)
 
  def reqSnip(name: String) = snip[Request](name) _
  
  val elem1 = reqSnip("elem1"){
    s => (s.state, <p>Elem1</p>)
  }
  
  val elem2 = snipNoState[Request]("elem2"){
    s => <p>{?("first", List("param")).text}</p>
  }
}

