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

def scalaCOptions(scalaVersion : String) = {
  if (scalaVersion.startsWith("2.12"))
    "-Ywarn-unused-import"
  else
    "-Wunused:imports"
}

def catsLibs = List("org.typelevel" %% "cats-core" % "2.12.0")
def catsEffectLibs = catsLibs ++ List("org.typelevel" %% "cats-effect" % "3.5.7")
def fs2Libs = List("co.fs2" %% "fs2-core" % "3.10.2")
def testLibs = List(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.17.1" % "test"
)

lazy val layout = (project in file("layout"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.layout"),
    libraryDependencies ++= catsLibs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  )

lazy val widget = (project in file("widget"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.widget"),
    libraryDependencies ++= catsLibs ++ testLibs ++ fs2Libs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  )

lazy val os = Option(System.getProperty("os.name", ""))
  .map(_.substring(0, 3).toLowerCase) match {
  case Some("win") => "windows"
  case Some("mac") => "macos"
  case _           => "linux"
}
lazy val lwjglVersion = "3.3.5"

lazy val impure = (project in file("impure"))
  .settings(
    name := "impure",
    idePackagePrefix := Some(s"$packagePrefix.impure"),
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  )

lazy val impureCats = (project in file("impure-cats"))
  .settings(
    name := "impure",
    idePackagePrefix := Some(s"$packagePrefix.impure.cats"),
    libraryDependencies ++= catsLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(impure)

lazy val impureCatsEffect = (project in file("impure-cats-effect"))
  .settings(
    name := "impure",
    idePackagePrefix := Some(s"$packagePrefix.impure.cats.effect"),
    libraryDependencies ++= catsEffectLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(impure)

lazy val freetype = (project in file("freetype"))
  .settings(
    name := "freetype",
    idePackagePrefix := Some(s"$packagePrefix.freetype"),
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ Seq(
      "org.lwjgl" % "lwjgl-freetype" % lwjglVersion,
      "org.lwjgl" % "lwjgl"        % lwjglVersion,
      "org.lwjgl" % "lwjgl-freetype" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl"        % lwjglVersion classifier s"natives-$os",
    ),
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(impure)

lazy val glfw = (project in file("glfw"))
  .settings(
    name := "glfw",
    idePackagePrefix := Some(s"$packagePrefix.glfw"),
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ Seq(
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion,
      "org.lwjgl" % "lwjgl"        % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl"        % lwjglVersion classifier s"natives-$os",
    ),
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(impure)

lazy val draw = (project in file("draw"))
  .settings(
    name := "draw",
    idePackagePrefix := Some(s"$packagePrefix.draw"),
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ Seq(
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
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(widget).dependsOn(impure)

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    idePackagePrefix := Some(s"$packagePrefix.example"),
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalafixOnCompile := true,
    scalacOptions += scalaCOptions(scalaVersion.value)
  ).dependsOn(widget).dependsOn(draw).dependsOn(layout)

