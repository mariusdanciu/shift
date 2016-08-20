
package net.shift.template
package test

import net.shift.common._
import State._
import scala.xml._
import net.shift.loc.Language
import scala.util.Success
import net.shift.io.IODefaults
import scala.util.Failure

import net.shift.io.IODefaults._

object TemplateTest extends App {

  val page =
    """
<!DOCTYPE html>
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
      
      <!-- snip:form1 -->
        <FORM action="http://somesite.com/prog/adduser" method="post">
          <P/>
          <LABEL for="firstname">First name: </LABEL>
          <INPUT type="text" id="firstname" value="<!-- inline:userInfo(firstName) -->"/><BR/>
          <LABEL for="lastname">Last name: </LABEL>
          <INPUT type="text" id="lastname" value="<!-- inline:userInfo(lastName) -->"/><BR/>
          <LABEL for="email">email: </LABEL>
          <INPUT type="text" value="<!-- inline:userInfo(email) -->"/><BR/>
          <INPUT type="radio" name="sex" value="Male"/>
          Male<BR/>
          <INPUT type="radio" name="sex" value="Female"/>
          Female<BR/>
          <INPUT type="submit" value="Send"/>
           <!-- snip:form2 -->
             what ?
           <!--end-->
          <INPUT type="reset"/>
          <P/>
        </FORM>
        
        <span> another node</span>
        <!--end-->
        
        <!--ceva comentariu-->
        
        <!--loc:user.name -->
        <!--inline:userInfo(lastName)-->
        
        <!-- snip:permissions (read, write) -->
        <div>
          <span>Sensitive content</span> 
          
          <span>Another sensitive content</span>
        </div>
        <!--end-->
        
        <!--template:footer  -->
      </body>
    </html>
"""
  import Snippet._

  val snippets = new DynamicContent[String] {
    override def inlines = List(
      inline("userInfo") {
        _.params match {
          case "firstName" :: _ => Success(("repl", "Marius"))
          case "lastName" :: _  => Success(("repl", "Danciu"))
          case _                => Success(("repl", "?"))
        }
      })

    def snippets = List(
      snip("form1") {
        s =>
          Success(("form", <div id="processed">{ s.node }</div>))
      },
      snip("form2") {
        s =>
          Success(("form", <div id="processed2">{ s.node }</div>))
      },
      snip("permissions") {
        s =>
          Console println s.params
          Success(("security", <div id="secured">{ s.node }</div>))
      })
  }
  implicit val tq: TemplateFinder = {
    case "head"   => Success("""<span>head from template</span>""")
    case "footer" => Success("""<span>FOOTER</span>""")
    case _        => Failure(new Exception("Not found"))
  }

  val r = Template().run(page, snippets, PageState("", Language("en"), None))
  println(r)
}
