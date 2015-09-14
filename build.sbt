name := """rdfl-playoffs"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  ws,
  "net.debasishg" %% "redisclient" % "3.0",
  "com.github.nscala-time" %% "nscala-time" % "2.2.0"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )
