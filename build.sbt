name := "assignment"

version := "0.1"

scalaVersion := "2.13.1"

lazy val assignment = project.in(file(".")).settings(libraryDependencies ++= Dependencies.all)
