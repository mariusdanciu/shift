import sbt._
import Keys._

object ShiftNettyBuild extends Build {
    lazy val root = Project(id = "shift",
                            base = file(".")) aggregate(shift_common, engine, shift_netty)

    lazy val shift_common = Project(id = "shift-common",
                              base = file("shift-common"))

    lazy val engine = Project(id = "engine",
                              base = file("engine")) dependsOn (shift_common)

    lazy val shift_netty = Project(id = "shift-netty",
                           base = file("shift-netty")) dependsOn (engine)
}
