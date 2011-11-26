import sbt._
import Keys._

object ShiftNettyBuild extends Build {
  lazy val root = Project(id = "shift",
                          base = file(".")) aggregate(shift_common, shift_engine, shift_netty)

  lazy val shift_common = Project(id = "shift-common",
				  base = file("shift-common"))

  lazy val shift_io = Project(id = "shift-io",
				  base = file("shift-io"))

  lazy val shift_engine = Project(id = "shift-engine",
				  base = file("shift-engine")) dependsOn (shift_common, shift_template)

  lazy val shift_template = Project(id = "shift-template",
				    base = file("shift-template")) dependsOn (shift_common, shift_io)

  lazy val shift_netty = Project(id = "shift-netty",
				 base = file("shift-netty")) dependsOn (shift_engine)

  lazy val shift_demo = Project(id = "demo",
				base = file("examples/demo")) dependsOn (shift_netty)


}
