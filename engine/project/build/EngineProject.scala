import sbt._
import java.io._
import scala.xml._

class EngineProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {

  val jeeweb = "javax.servlet" % "servlet-api" % "2.5" 
 
  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.1")

  override def compileOptions = super.compileOptions ++ compileOptions("-P:continuations:enable")

}
