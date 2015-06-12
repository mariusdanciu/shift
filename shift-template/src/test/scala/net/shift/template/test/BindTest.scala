package net.shift
package template
package test

import net.shift.common.BNodeImplicits._
import net.shift.common.XmlUtils._
import net.shift.common.BNode
import Binds._

object BindTest extends App {
  val xml = <div class="images">
              <ul>
                <f:li>
                  <f:img class="thumb" id="ah"/>
                </f:li>
                <span id="1"></span>
              </ul>
            </div>

  val images = List("a", "b", "c")

  val res = bind(xml) {
    case BNode("ul", _, childs)   => BNode("ul") / childs
    case BNode("f:li", _, childs) => childs
    case "1" HasId a              => <b>1</b>
    case BNode("f:img", HasClass("thumb", a), _) =>
      images map { i =>
        <li>
          { <img src={ i }></img> }
        </li>
      }
  }

  println(res)
}