package net.shift

import scala.xml.NodeSeq
import common._

package object template {
  
  type SnippetFunc = NodeSeq => NodeSeq
}