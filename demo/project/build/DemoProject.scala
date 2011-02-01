import sbt._

class DemoProject(info: ProjectInfo) extends DefaultWebProject(info)  {

  val jetty7 = "org.eclipse.jetty" % "jetty-webapp" % "7.0.2.RC0" % "test"

}
