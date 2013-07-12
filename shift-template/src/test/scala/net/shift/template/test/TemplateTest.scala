
package net.shift.template
package test

import net.shift.common._
import State._
import org.scalatest._
import scala.xml._

object TemplateTest extends App {
  val page = <html>
               <head>
               </head>
               <body>
                 <FORM class="form1" action="http://somesite.com/prog/adduser" method="post">
                   <P/>
                   <LABEL for="firstname">First name: </LABEL>
                   <INPUT type="text" id="firstname"/><BR/>
                   <LABEL for="lastname">Last name: </LABEL>
                   <INPUT type="text" id="lastname"/><BR/>
                   <LABEL for="email">email: </LABEL>
                   <INPUT class="email" type="text"/><BR/>
                   <INPUT type="radio" name="sex" value="Male"/>
                   Male<BR/>
                   <INPUT type="radio" name="sex" value="Female"/>
                   Female<BR/>
                   <INPUT type="submit" value="Send"/>
                   <INPUT type="reset"/>
                   <P/>
                 </FORM>
               </body>
             </html>

  val snippets = new DynamicContent[String] {
    def snippets = List(
      Snippet("form1", state {
        s =>
          Console println s.req
          val ex = s.node match {
            case SnipNode(name, attrs, childs) => <form>{ childs }</form>
          }
          Some(PageState("form", ex), ex)
      }),
      Snippet("email", state {
        s =>
          Console println s.req
          val ex = s.node match {
            case SnipNode(name, attrs, childs) => <input type="text" id="email1">Type email here</input>
          }
          Some(PageState("email", ex), ex)
      }))
  }

  val res = new Template[String](snippets, Selectors.byClassAttr[PageState[String]])

  val e = for {
    t <- res.run(page)
  } yield {
    Template.mkString(t)
  }
  for {
    c <- e(PageState("start", NodeSeq.Empty))
  } yield {
    Console println c._2
  }
}