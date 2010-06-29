import sbt._

class ShiftParent(info: ProjectInfo) extends ParentProject(info) {

   val engine = project("engine", "Shift Engine")
}
