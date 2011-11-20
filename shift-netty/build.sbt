name := "shift-netty"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies += "org.jboss.netty" % "netty" % "3.2.6.Final"

resolvers ++= Seq(
  "Netty repository" at "https://repository.jboss.org/nexus/content/repositories/releases"
)

