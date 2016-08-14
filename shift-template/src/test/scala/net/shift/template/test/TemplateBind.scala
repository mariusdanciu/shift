package net.shift
package template.test

import scala.util.Success
import scala.xml.NodeSeq
import net.shift.loc.Language
import net.shift.template.DynamicContent
import net.shift.template.Selectors
import net.shift.template.SnipState
import net.shift.template.Template
import net.shift.template.TemplateFinder
import net.shift.template.Snippet
import net.shift.template.PageState
import net.shift.io.IODefaults

object TemplateBind extends App with Selectors with IODefaults {

  val head = """
        <link rel="apple-touch-icon image_src" href="//cdn.sstatic.net/stackoverflow/img/apple-touch-icon.png?v=fd7230a85918"/>
        <link rel="search" type="application/opensearchdescription+xml" title="Stack Overflow" href="/opensearch.xml"/>
        <meta name="twitter:card" content="summary"/>
    """

  val place = """
        <div id="place">
          it worked
          <img src="/a" data-unique="src"/>
        </div>
    """

  val second = """
         <div id="second">
          it worked again
          <img src="/a" data-unique="src"/>
        </div>
    """

  val page = """
    <html>
      <head>
        <!-- template:head -->
        <link rel="shortcut icon" href="//cdn.sstatic.net/stackoverflow/img/favicon.ico?v=038622610830"/>
      </head>
      
      <body>
        <!-- template:place -->
        <span> In the middle</span>
        <!-- template:second -->
      </body>
    </html>
"""
  implicit val finder: TemplateFinder = {
    case "place" => Success(NodeSeq.Empty)
    case "second" => Success(NodeSeq.Empty)
  }

  val content = new DynamicContent[String] {
    def snippets = List(elem1)

    import Snippet._

    def reqSnip(name: String) = snip[String](name) _

    val elem1 = reqSnip("elem1") {
      s =>
        Success((s.state.initialState, s.node ++ <p>Elem1</p>))
    }
  }


}