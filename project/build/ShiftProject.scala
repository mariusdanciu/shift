import sbt._

class ShiftProject(info: ProjectInfo) extends DefaultProject(info) {
  lazy val m2repo = DefaultMavenRepository
  val jeeweb = "javax.servlet" % "servlet-api" % "2.5" 
}
