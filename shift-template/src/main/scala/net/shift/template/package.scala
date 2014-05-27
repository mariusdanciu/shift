package net.shift

import scala.xml.NodeSeq
import common._
import scala.util.Try

package object template {
  type TemplateFinder = String => Try[NodeSeq]
}