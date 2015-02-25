name := "shift-common"

organization := "shift"

version := buildProps.getProperty("version") + "." + buildProps.getProperty("build")

scalaVersion := "2.11.2"

libraryDependencies += "org.json4s" % "json4s-core_2.11" % "3.2.10"

libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.2.10"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

