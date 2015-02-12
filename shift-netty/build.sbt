name := "shift-netty"

organization := "shift"

version := buildProps.getProperty("version") + "." + buildProps.getProperty("build")

scalaVersion := "2.11.2"

libraryDependencies += "org.jboss.netty" % "netty" % "3.2.6.Final"

resolvers ++= Seq(
  "Netty repository" at "https://repository.jboss.org/nexus/content/repositories/releases"
)

