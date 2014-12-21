package net.shift
package template
package test

import Binds._
import net.shift.common.NodeOps._
import net.shift.common.NodeOps
import net.shift.common.XmlUtils

object BindTest extends App with XmlUtils {
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
    case "ul" attributes _ / childs   => node("ul") / childs
    case "f:li" attributes a / childs => childs
    case HasId("1", a)                => <b>1</b>
    case "f:img" attributes HasClass("thumb", a) =>
      images map { i =>
        <li>
          { <img src={ i }></img> }
        </li>
      }
  }

  println(res)
}