package net.shift
package demo

import scala.xml._
import template._
import engine.http._
import Snippet._
import loc.Loc._
import net.shift.loc.Language
import scala.util.Success
import net.shift.io.IODefaults

import net.shift.io.IODefaults._

object FirstPage extends DynamicContent[Request] {

  val ? = loc("test", Language("ro")) _

  def snippets = List(elem1, elem2, test)

  def reqSnip(name: String) = snip[Request](name) _

  val elem1 = reqSnip("elem1") {
    s =>
      println(s.node)
      Success((s.state.initialState, <p>Elem1</p> ++ s.node))
  }

  val elem2 = snipNoState[Request]("elem2") {
    s => Success(<p>{ ?("first", List("param")).text }</p>)
  }

  val test = snipNoState[Request]("test") {
    s =>
      println("in test")
      Success(s.node ++ Text("Executed"))
  }
}

