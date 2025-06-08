import sbt.Keys.libraryDependencies
import sbtide.Keys.idePackagePrefix

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

sys.env.get("LD_LIBRARY_PATH") match {
  case Some(libPath) =>
    javaOptions += s"-Djava.library.path=$libPath"
  case _ => javaOptions += ""
}


addCompilerPlugin("org.wartremover" %% "wartremover" % "3.3.4" cross CrossVersion.full)

scalacOptions += "-Xkind-projector"
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

def scalaCOptions(scalaVersion : String) =
  List(
    if (scalaVersion.startsWith("2.12"))
      "-Ywarn-unused-import"
    else
      "-Wunused:imports",
    "-Xkind-projector"
  )

def catsLibs = List("org.typelevel" %% "cats-core" % "2.13.0")
def catsEffectLibs = catsLibs ++ List("org.typelevel" %% "cats-effect" % "3.6.1")
def fs2Libs = List("co.fs2" %% "fs2-core" % "3.12.0")
def testLibs = List(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"
)

val catnip = (project in file("catnip"))
  .settings(
    name := "catnip",
    idePackagePrefix := Some("catnip"),
    libraryDependencies ++= catsLibs ++ testLibs,
    wartremoverErrors := Warts.all,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  )

val catnipEffect = (project in file("catnip-cats-effect"))
  .settings(
    name := "catnip-cats-effect",
    idePackagePrefix := Some("catnip.cats.effect"),
    libraryDependencies ++= catsEffectLibs ++ testLibs,
    wartremoverErrors := Warts.all,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip)

lazy val layout = (project in file("layout"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.layout"),
    libraryDependencies ++= catsLibs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  )

lazy val widget = (project in file("widget"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.widget"),
    libraryDependencies ++= catsLibs ++ testLibs ++ fs2Libs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip)

lazy val lwjglVersion = "3.3.6"

lazy val os = Option(System.getProperty("os.name", ""))
  .map(_.substring(0, 3).toLowerCase) match {
  case Some("win") => "windows"
  case Some("mac") => "macos"
  case _           => "linux"
}


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
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip, catnipEffect)

lazy val skijaLibs = List(
  "io.github.humbleui" % "skija-shared" % "0.116.4",
  "io.github.humbleui" % s"skija-$os-x64" % "0.116.4",
)

lazy val draw = (project in file("draw"))
  .settings(
    name := "draw",
    //idePackagePrefix := Some(s"$packagePrefix.draw"),
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ Seq(
      "org.lwjgl" % "lwjgl"        % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-vulkan" % lwjglVersion,
      "org.lwjgl" % "lwjgl-shaderc" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion,
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion,
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion,
      "org.lwjgl" % "lwjgl"        % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-shaderc" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion classifier s"natives-$os",
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion classifier s"natives-$os",
      "org.joml" % "joml" % "1.10.8",
    ) ++ skijaLibs,
    mainClass := Some("io.github.humbleui.skija.example.Main"),
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(widget).dependsOn(catnip, glfw)

lazy val loops = (project in file("loops"))
  .settings(
    name := "loops",
    idePackagePrefix := Some(s"$packagePrefix.loops"),
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  )

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    idePackagePrefix := Some(s"$packagePrefix.example"),
    libraryDependencies ++= catsLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List("org.scala-lang.modules" %% "scala-swing" % "3.0.0"),
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value),
    mainClass := Some("me.katze.gui4s.example.SkijaAppExample")
  )
  .dependsOn(widget, draw, layout, loops, catnip, catnipEffect, glfw)

