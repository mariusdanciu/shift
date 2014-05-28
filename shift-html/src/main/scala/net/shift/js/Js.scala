package net.shift.js

trait Js {
  def toJsString: String
}

trait JsExp extends Js { self =>
  def wrap: JsExp = new JsExp {
    def toJsString = s"( ${self.toJsString} )"
  }

  def stmt: JsExp = new JsExp {
    def toJsString = s"${self.toJsString}; "
  }

  def apply: JsExp = new JsExp {
    def toJsString = s"${self.toJsString}() "
  }
}

trait JsDecl extends Js {
}

case class JsVar(name: String, value: JsExp) extends JsDecl {
  def toJsString = s"var $name = ${value.toJsString};"
}

case class JsRet(value: JsExp) extends JsDecl {
  def toJsString = s"return ${value.toJsString};"
}

object JsFunc {
  def apply(body: Js) = new JsFunc(Nil, body)
}

case class JsFunc(params: Seq[String], body: Js) extends JsExp {
  def toJsString = {
    s"function (${params.mkString(", ")}) { ${body.toJsString} }";
  }
}

case class JsApplyExp(name: JsExp, params: JsExp*) extends JsExp {
  def toJsString = {
    val p = params map { _ toJsString }
    s"${name.toJsString}(${p.mkString(", ")})"
  }
}

case class JsApply(name: String, params: JsExp*) extends JsExp {
  def toJsString = {
    val p = params map { _ toJsString }
    s"$name(${p.mkString(", ")})"
  }
}

case class JsString(value: String) extends JsExp {

  def toJsString = s""""${value.replace("\"", "\\\"")}"""";
}

case class JsonString(value: String) extends JsExp {
  def toJsString = s"${value.replace("\"", "\\\"")}";
}


case class JsChain(elems: JsExp*) extends JsExp {
  def toJsString = {
    val p = elems map { _ toJsString }
    s"${p.mkString(".")}"
  }
}

case class JsStatement(exp: JsExp*) extends Js {self =>
  def toJsString = {
    ("" /: exp)((acc, l) => acc + l.toJsString + "; ")
  }

  def ~(stmt: JsStatement): JsStatement = new JsStatement {
    override def toJsString = {
      self.toJsString + stmt.toJsString
    }
  }
}

object JsDsl {
  implicit def str2JsStr(in: String): JsString = JsString(in)

  implicit class Chaining(in: JsExp) {
    def ~(other: JsExp): JsChain = JsChain(in, other)
  }

  def $(exp: JsExp) = JsApply("$", exp)

  def apply(name: JsExp, exp: JsExp*) = JsApplyExp(name, exp: _*)

  def apply(name: String, exp: JsExp*) = JsApply(name, exp: _*)

  def func(params: String*)(body: Js) = new JsExp {
    def toJsString = {
      s"function (${params.mkString(", ")}) { ${body.toJsString} }";
    }
  }

}

object Main extends App {

  import JsDsl._

  val s = func() {
    $("div") ~
      apply("css", "background-color", "blue") ~
      apply("css", "background-color", "red") stmt
  }.wrap.apply

  println(s.toJsString)

}