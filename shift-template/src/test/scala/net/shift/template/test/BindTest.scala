package net.shift
package template
package test

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
    case "ul" > (_ / childs) => <ul>{ childs }</ul>
    case "f:li" > (_ / childs) => childs
    case HasId("1") => <b>1</b>
    case "f:img" > (HasClass("thumb") / _) =>
      images map { i =>
        <li>
          { <img src={ i }></img> }
        </li>
      }
  }

  println(res)

}