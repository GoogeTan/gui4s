ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"

val packagePrefix = "me.katze.gui4s"

lazy val root = (project in file("."))
  .settings(
    name := "gui4s",
    idePackagePrefix := Some("me.katze")
  )

lazy val layout = (project in file("layout"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.layout")
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"