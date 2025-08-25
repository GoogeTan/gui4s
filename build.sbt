import sbt.Keys.libraryDependencies
import sbtide.Keys.idePackagePrefix

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

// for running glfw in sbt shell
example/run/fork := true
example/run/javaOptions ++= Seq("-XstartOnFirstThread")
// end for sbt shell

// for nix os
sys.env.get("LD_LIBRARY_PATH") match {
  case Some(libPath) =>
    javaOptions += s"-Djava.library.path=$libPath"
  case _ => javaOptions += ""
}
// end for nix os

addCompilerPlugin("org.wartremover" %% "wartremover" % "3.3.4" cross CrossVersion.full)

scalacOptions += "-Xkind-projector"
scalacOptions += "-P:wartremover:traverser:org.wartremover.warts.Unsafe"
scalacOptions += "-experimental"

inThisBuild(
  List(
    semanticdbEnabled := true, // enable SemanticDB for scalafix
  )
)

val packagePrefix = "gui4s"

def scalaCOptions(scalaVersion : String) =
  List(
    if (scalaVersion.startsWith("2.12"))
      "-Ywarn-unused-import"
    else
      "-Wunused:imports",
    "-Xkind-projector"
  )

def catsLibs = List("org.typelevel" %% "cats-core" % "2.13.0")
def catsEffectLibs = catsLibs ++ List("org.typelevel" %% "cats-effect" % "3.6.3")
def fs2Libs = List("co.fs2" %% "fs2-core" % "3.12.0")
def testLibs = List(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test",
  "org.typelevel" %% "cats-laws" % "2.13.0" % Test,
  "org.typelevel" %% "discipline-core" % "1.7.0" % Test,
  "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test
)

val geometry = (project in file("core/geometry"))
  .settings(
    name := "geometry",
    idePackagePrefix := Some(packagePrefix + ".core.geometry"),
    libraryDependencies ++= catsLibs ++ testLibs,
    wartremoverErrors := Warts.all,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
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
    idePackagePrefix := Some("catnip.effect"),
    libraryDependencies ++= catsEffectLibs ++ testLibs,
    wartremoverErrors := Warts.all,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip)

lazy val layout = (project in file("core/layout"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.core.layout"),
    libraryDependencies ++= catsLibs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(geometry, catnip)

lazy val widget = (project in file("core/widget"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.core.widget"),
    libraryDependencies ++= catsLibs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip)

lazy val widgetLibrary = (project in file("desktop/widgetLibrary"))
  .settings(
    name := "layout",
    idePackagePrefix := Some(s"$packagePrefix.decktop.widget.library"),
    libraryDependencies ++= catsLibs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip, widget, layout, geometry)

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
  ).dependsOn(catnip, catnipEffect, geometry)

lazy val skijaLibs = List(
  "io.github.humbleui" % "skija-shared" % "0.116.4",
  "io.github.humbleui" % s"skija-$os-x64" % "0.116.4",
)

lazy val draw = (project in file("desktop/skija"))
  .settings(
    name := "draw",
    idePackagePrefix := Some(s"$packagePrefix.desktop.skija"),
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ skijaLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.allBut(Warts.all*),
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(catnip, glfw)

lazy val loops = (project in file("core/loops"))
  .settings(
    name := "loops",
    idePackagePrefix := Some(s"$packagePrefix.core.loops"),
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  )
  .dependsOn(geometry, catnip)

lazy val genericDevKit = (project in file("core/generickit"))
  .settings(
    name := "kit-generic",
    idePackagePrefix := Some(s"$packagePrefix.core.kit"),
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(widget, draw, layout, loops, catnip, catnipEffect, glfw, widgetLibrary)

lazy val desktopCatsDevKit = (project in file("desktop/kit/cats"))
  .settings(
    name := "kit-desctop-cats",
    idePackagePrefix := Some(s"$packagePrefix.desktop.kit.cats"),
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value)
  ).dependsOn(widget, draw, layout, loops, catnip, catnipEffect, glfw, widgetLibrary, genericDevKit)

lazy val example = (project in file("desktop/example/cats"))
  .settings(
    name := "example",
    idePackagePrefix := Some(s"$packagePrefix.desktop.example.cats"),
    libraryDependencies ++= catsLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List(
      "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6",
      "co.fs2" %% "fs2-core" % "3.12.0",
      "co.fs2" %% "fs2-io" % "3.12.0",
      "org.http4s" %% "http4s-ember-client" % "0.23.30",
      "org.http4s" %% "http4s-core" % "0.23.30"
    ),
    coverageEnabled := true,
    wartremoverErrors := Warts.unsafe,
    scalacOptions ++= scalaCOptions(scalaVersion.value),
    mainClass := Some("me.katze.gui4s.example.SkijaAppExample")
  )
  .dependsOn(widget, draw, layout, loops, catnip, catnipEffect, glfw, widgetLibrary, desktopCatsDevKit)

