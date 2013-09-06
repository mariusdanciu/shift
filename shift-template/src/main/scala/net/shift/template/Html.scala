package net.shift.template

import scala.xml._

object Html {

  def text(value: String = "") = <input type="text">{value}</input>
  def password = <input type="password"/>
  def radio(name: String, value: String) = <input type="radio" name={name} value={value} />
  def checkBox(name: String, value: String) = <input type="checkbox" name={name} value={value} />
  def submit(label: String) = <input type="submit" value={label} />
  def textArea(rows: Int, cols: Int) = <textarea rows={rows toString} cols={cols toString} />
  
}


