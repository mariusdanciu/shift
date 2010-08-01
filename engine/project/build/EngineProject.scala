import sbt._
import java.io._
import scala.xml._

class EngineProject(info: ProjectInfo) extends DefaultProject(info) {

  val jeeweb = "javax.servlet" % "servlet-api" % "2.5" 

}
