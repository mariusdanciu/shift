package net.shift
package template
package test

import net.shift.common.XmlImplicits._
import net.shift.common.XmlUtils._
import net.shift.common.Xml
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
    case Xml("ul", a, childs)   => Xml("ul") % (a + ("src", "url")) / childs
    case Xml("f:li", _, childs) => childs
    case "1" HasId a            => <b>1</b>
    case Xml("f:img", HasClass("thumb", a), _) =>
      images map { i =>
        <li>
          { <img src={ i }></img> }
        </li>
      }
  }

  println(res)
}