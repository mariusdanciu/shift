package net.shift
package template

import shiftdefs._
import scala.xml._
import util.Util._

case class BindMeta(attrs: MetaData, children: NodeSeq)
case class ToBind(name: String, meta: BindMeta)

trait Binds {

  def bind(pref: String, xml: NodeSeq)(bindFunc: ToBind ?=> NodeSeq): NodeSeq = {
    xml flatMap {
      case e: Document => bind(pref, e children)(bindFunc)
      case Group(nodes) => bind(pref, nodes)(bindFunc)
      case Elem(prefix, label, attrs, ns, childs @ _*) if (prefix == pref) =>
        applyPf(new ToBind(label, BindMeta(attrs, childs)))(bindFunc) getOrElse NodeSeq.Empty
      case Elem(prefix, label, attrs, ns, childs @ _*) if (childs != null && !childs.isEmpty) =>
        Elem(prefix, label, attrs, ns, bind(pref, childs)(bindFunc).theSeq : _*)
      case e => e 
    }
  }

}

object Binds extends Binds {
  val ~ = ToBind
  val & = BindMeta
}

object Main1 {
  import Binds._

  def main(args: Array[String]) = {
    val node = <div>
      <f:input id="12" > 
          <span>test</span>
      </f:input>
      <f:some/>
    </div>
   
    val res = bind("f", node) {
      case "input" ~ (attrs & childs) => <input name="Add some text please">{childs}</input> % attrs
      case "some" ~ _ => <span>some</span>
    }

    println(res)
  }

}
