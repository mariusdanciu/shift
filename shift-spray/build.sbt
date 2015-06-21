name := "shift-spray"

organization := "shift"

version := buildProps.getProperty("version") + "." + buildProps.getProperty("build")

scalaVersion := "2.11.2"

libraryDependencies ++= Seq("io.spray" % "spray-can_2.11" % "1.3.3", 
                            "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3")

resolvers += "spray repo" at "http://repo.spray.io"

