name := "thunder"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.typelevel" %% "cats" % "0.7.2",
  "org.typelevel" %% "algebra" % "0.5.1"
)

scalacOptions += "-feature"