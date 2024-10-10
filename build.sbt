import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.1"

val packagePrefix = "me.katze.gui4s"
/*
lazy val root = (project in file("."))
  .settings(
    name := "gui4s",
    idePackagePrefix := Some("me.katze")
  )*/

lazy val layout = (project in file("layout"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.layout"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.1" % "test",
      "io.github.iltotore" %% "iron" % "2.4.0",
    ),
    coverageEnabled := true
  )

lazy val widget = (project in file("widget"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.widget"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.1" % "test",
      "co.fs2" %% "fs2-core" % "3.10.2",
    ),
    coverageEnabled := true
  )

lazy val draw = (project in file("draw"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.draw"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.1" % "test",
      "co.fs2" %% "fs2-core" % "3.10.2"
    ),
    mainClass := Some("me.katze.gui4s.example.lwjgl.Example"),
    coverageEnabled := true
  ).dependsOn(widget)

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    idePackagePrefix := Some(s"$packagePrefix.example"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "co.fs2" %% "fs2-core" % "3.10.2",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.1" % "test",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    ),
    coverageEnabled := true,
  ).dependsOn(widget).dependsOn(draw).dependsOn(layout)
