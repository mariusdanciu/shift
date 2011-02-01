package net.shift
package http

object Path {
  def apply(str: String): Path = {
    val endSlash = str endsWith "/"
    val abs = str startsWith "/"
    var uri = str split "/" toList

    uri = if (abs && ! uri.isEmpty) uri tail else uri

    new Path(uri, endSlash)
  }
 
  val empty = Path("") 
}

sealed case class Path(val parts: List[String], val endSlash: Boolean) {


  def / (part : String) = new Path(parts :+ part, endSlash)

  def matches(other: Path): Boolean = this.parts == other.parts

  override def toString() = parts mkString("/")
}


