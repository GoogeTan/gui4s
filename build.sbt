import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.1"

addCompilerPlugin("org.wartremover" %% "wartremover" % "3.2.5" cross CrossVersion.full)

scalacOptions += "-P:wartremover:traverser:org.wartremover.warts.Unsafe"

inThisBuild(
  List(
    semanticdbEnabled := true, // enable SemanticDB for scalafix
  )
)

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
      "org.scalacheck" %% "scalacheck" % "1.17.1" % "test"
    ),
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions +=
      {
        if (scalaVersion.value.startsWith("2.12"))
          "-Ywarn-unused-import"
        else
          "-Wunused:imports"
      }

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
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions +=
      {
        if (scalaVersion.value.startsWith("2.12"))
          "-Ywarn-unused-import"
        else
          "-Wunused:imports"
      }
  )

lazy val os = Option(System.getProperty("os.name", ""))
  .map(_.substring(0, 3).toLowerCase) match {
  case Some("win") => "windows"
  case Some("mac") => "macos"
  case _           => "linux"
}
lazy val lwjglVersion = "3.3.5"

lazy val draw = (project in file("draw"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.draw"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalacheck" %% "scalacheck" % "1.18.1" % "test",
      "co.fs2" %% "fs2-core" % "3.11.0",

      "org.lwjgl" % "lwjgl"        % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion,
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion,
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion,
      "org.lwjgl" % "lwjgl"        % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion classifier s"natives-$os"
    ),
    mainClass := Some("me.katze.gui4s.example.lwjgl.Example"),
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions +=
      {
        if (scalaVersion.value.startsWith("2.12"))
          "-Ywarn-unused-import"
        else
          "-Wunused:imports"
      }
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
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions +=
      {
        if (scalaVersion.value.startsWith("2.12"))
          "-Ywarn-unused-import"
        else
          "-Wunused:imports"
      }
  ).dependsOn(widget).dependsOn(draw).dependsOn(layout)

