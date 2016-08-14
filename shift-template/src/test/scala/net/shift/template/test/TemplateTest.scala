
package net.shift.template
package test

import net.shift.common._
import State._
import scala.xml._
import net.shift.loc.Language
import scala.util.Success
import net.shift.io.IODefaults

object TemplateTest extends App with Selectors with IODefaults {
  val page =
    """
<!--[if lt IE 7]>      <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
<!--[if IE 7]>         <html class="no-js lt-ie9 lt-ie8"> <![endif]-->
<!--[if IE 8]>         <html class="no-js lt-ie9"> <![endif]-->
<!--[if gt IE 8]><!-->
<html class="no-js">
<!--<![endif]-->
      <head>
      </head>
      <body>
      
      <!--template:head -->
      
      <!--snip:form1 -->
        <FORM action="http://somesite.com/prog/adduser" method="post">
          <P/>
          <LABEL for="firstname">First name: </LABEL>
          <INPUT type="text" id="firstname"/><BR/>
          <LABEL for="lastname">Last name: </LABEL>
          <INPUT type="text" id="lastname"/><BR/>
          <LABEL for="email">email: </LABEL>
          <INPUT type="text"/><BR/>
          <INPUT type="radio" name="sex" value="Male"/>
          Male<BR/>
          <INPUT type="radio" name="sex" value="Female"/>
          Female<BR/>
          <INPUT type="submit" value="Send"/>
          <INPUT type="reset"/>
          <P/>
        </FORM>
        <!--end-->
        
        <!--ceva comentariu-->
        
        <!--loc:user.name -->
        
        <!--snip:permission-->
        <div>
          <span>Sensitive content</span> 
          
          <span>ANother sensitive content</span>
        </div>
        <!--end-->
        
        <!--template:footer  -->
      </body>
    </html>
"""
  import Snippet._
  import Template._

  val snippets = new DynamicContent[String] {
    def snippets = List(
      snip("form1") {
        s =>
          val SnipNode(name, attrs, childs) = s.node
          Success(("form", <form id="processed">{ childs }</form>))
      },
      snip("email") {
        s =>
          Console println s.state
          Success(("email", <input type="text" id="email1">Type email here</input>))
      })
  }
  implicit val tq: TemplateQuery = {
    case "head"   => Success("""<span>from template</span>""")
    case "footer" => Success("""<span>FOOTER</span>""")
  }

  val r = new StringTemplate().run(page, snippets, PageState("", Language("en"), None))
  println(r)
}
