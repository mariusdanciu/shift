import sbt._

class ShiftParent(info: ProjectInfo) extends ParentProject(info) {

   lazy val engine = project("engine")

   lazy val demo = project("demo", engine)
}
