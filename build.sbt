import sbt.Keys.libraryDependencies

scalaVersion := "2.12.2"

name := "tutorial"

libraryDependencies ++= Seq(
   "org.scalatest" %% "scalatest" % "3.0.1" % "test"
   )

