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

  val template = <html>
                   <head>
                     <link rel="shortcut icon" href="//cdn.sstatic.net/stackoverflow/img/favicon.ico?v=038622610830"/>
                   </head>
                   <body>
                     <h1>before</h1>
                     <div id="place"/>
                     <h1>after</h1>
                     <div id="second"/>
                     <img src="/a" data-unique="src"/>
                   </body>
                 </html>

  val page =
    <html data-template="wrap">
      <head>
        <link rel="apple-touch-icon image_src" href="//cdn.sstatic.net/stackoverflow/img/apple-touch-icon.png?v=fd7230a85918"/>
        <link rel="search" type="application/opensearchdescription+xml" title="Stack Overflow" href="/opensearch.xml"/>
        <meta name="twitter:card" content="summary"/>
      </head>
      <body>
        <div id="place" data-snip="elem1">
          it worked
          <img src="/a" data-unique="src"/>
        </div>
        <div id="second" data-snip="elem1">
          it worked again
          <img src="/a" data-unique="src"/>
        </div>
      </body>
    </html>

  implicit val finder: TemplateFinder = name => Success(template)
  implicit val selector = bySnippetAttr[SnipState[String]]

  val content = new DynamicContent[String] {
    def snippets = List(elem1)

    import Snippet._

    def reqSnip(name: String) = snip[String](name) _

    val elem1 = reqSnip("elem1") {
      s =>
        Success((s.state.initialState, s.node ++ <p>Elem1</p>))
    }
  }
  lazy val t = Template[String](content) run page
  val res = for {
    c <- t(SnipState(PageState("", Language("ro"), None), NodeSeq.Empty))
  } yield c

  println(res)

}