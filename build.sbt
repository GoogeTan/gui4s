import sbt.Keys.libraryDependencies
import sbtide.Keys.idePackagePrefix

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

// for nix os
ThisBuild / javaOptions ++= sys.env.get("LD_LIBRARY_PATH").map(p => Seq(s"-Djava.library.path=$p")).getOrElse(Seq())
// end for nix os

Global / excludeLintKeys += idePackagePrefix

  
def catsLibs = List("org.typelevel" %% "cats-core" % "2.13.0")
def catsEffectLibs = catsLibs ++ List("org.typelevel" %% "cats-effect" % "3.6.3")
def fs2Libs = List("co.fs2" %% "fs2-core" % "3.12.0")

val zioLibs = List(
 "dev.zio" %% "zio" % "2.1.20",
 "dev.zio" %% "zio-interop-cats" % "23.1.0.5"
)

def testLibs = List(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test",
  "org.typelevel" %% "cats-laws" % "2.13.0" % Test,
  "org.typelevel" %% "discipline-core" % "1.7.0" % Test,
  "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test
)
lazy val lwjglVersion = "3.3.6"
lazy val arch = if (System.getProperty("os.arch").contains("aarch64")) "arm64" else "x64"
lazy val os = Option(System.getProperty("os.name", ""))
  .map(_.substring(0, 3).toLowerCase) match {
  case Some("win") => "windows"
  case Some("mac") => "macos"
  case _           => "linux"
}

lazy val glfwLibs = List(
  "org.lwjgl" % "lwjgl-glfw" % lwjglVersion,
  "org.lwjgl" % "lwjgl"        % lwjglVersion,
  "org.lwjgl" % "lwjgl-glfw" % lwjglVersion classifier s"natives-$os",
  "org.lwjgl" % "lwjgl"        % lwjglVersion classifier s"natives-$os",
)

lazy val skijaLibs = List(
  "io.github.humbleui" % "skija-shared" % "0.116.4",
  "io.github.humbleui" % s"skija-$os-$arch" % "0.116.4",
)

def commonSettings(nameIn : String, pkg : String) =
  List(
     name := nameIn,
     idePackagePrefix := Some(pkg),
     wartremoverErrors := Warts.allBut(Wart.ListAppend, Wart.Overloading, Wart.Nothing, Wart.DefaultArguments),
     scalacOptions ++= List("-Wunused:imports", "-Xkind-projector")
  )

val packagePrefix = "gui4s"

val geometry = (project in file("core/geometry"))
  .settings(commonSettings("geometry", packagePrefix + ".core.geometry"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)

val catnip = (project in file("catnip"))
  .settings(commonSettings("catnip", "catnip"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)

val catnipCatsEffect = (project in file("catnip-cats-effect"))
  .settings(commonSettings("catnip-cats-effect", "catnip.effect"))
  .settings(libraryDependencies ++= catsEffectLibs ++ testLibs)
  .dependsOn(catnip)

val catnipZio = (project in file("catnip-zio"))
  .settings(commonSettings("catnip-zio", "catnip.zio"))
  .settings(libraryDependencies ++= zioLibs ++ testLibs)
  .dependsOn(catnip)

lazy val layout = (project in file("core/layout"))
  .settings(commonSettings("layout", s"$packagePrefix.core.layout"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)
  .dependsOn(geometry, catnip)

lazy val widget = (project in file("core/widget"))
  .settings(commonSettings("widget", s"$packagePrefix.core.widget"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)
  .dependsOn(catnip)

lazy val glfw = (project in file("glfw"))
  .settings(commonSettings("glfw", s"$packagePrefix.glfw"))
  .settings(libraryDependencies ++= catsEffectLibs ++ testLibs ++ glfwLibs)
  .dependsOn(catnip, catnipCatsEffect, geometry)// TODO Может стоит убрать кетс эффект.

lazy val desktopSkijaBindings = (project in file("desktop/skija"))
  .settings(commonSettings("desktop-skija-bindings", s"$packagePrefix.desktop.skija"))
  .settings(libraryDependencies ++= catsEffectLibs ++ testLibs ++ skijaLibs)
  .dependsOn(catnip, glfw, layout)

lazy val loops = (project in file("core/loop"))
  .settings(commonSettings("loop", s"$packagePrefix.core.loop"))
  .settings(libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs)
  .dependsOn(catnip)

lazy val desktopWidgetLibrary = (project in file("desktop/widgetLibrary"))
  .settings(commonSettings("desktop-widget-library", s"$packagePrefix.desktop.widget.library"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)
  .dependsOn(catnip, widget, layout, geometry)

lazy val commonDevKit = (project in file("core/kit"))
  .settings(commonSettings("core-kit", s"$packagePrefix.core.kit"))
  .settings(libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs)
  .dependsOn(widget, layout, loops, catnip)

lazy val desktopCommonDevKit = (project in file("desktop/kit/common"))
  .settings(commonSettings("desktop-kit-common", s"$packagePrefix.desktop.kit.common"))
  .settings(libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs)
  .dependsOn(widget, desktopSkijaBindings, layout, loops, glfw, catnip, desktopWidgetLibrary, commonDevKit)

lazy val desktopCatsDevKit = (project in file("desktop/kit/cats"))
  .settings(commonSettings("desktop-kit-cats", s"$packagePrefix.desktop.kit.cats"))
  .settings(libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs)
  .dependsOn(desktopCommonDevKit)
  .dependsOn(catnipCatsEffect)

lazy val desktopZioDevKit = (project in file("desktop/kit/zio"))
  .settings(commonSettings("desktop-kit-zio", s"$packagePrefix.desktop.kit.zio"))
  .settings(
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs ++ zioLibs,
    wartremoverErrors := Warts.allBut(Wart.ListAppend, Wart.Overloading, Wart.Nothing, Wart.DefaultArguments, Wart.Any)
  ).dependsOn(desktopCommonDevKit, catnipZio)

lazy val desktopCatsExample = (project in file("desktop/example/cats"))
  .settings(commonSettings("desktop-example-cats", s"$packagePrefix.desktop.example.cats"))
  .settings(
    libraryDependencies ++= catsLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List(
      "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6",
      "co.fs2" %% "fs2-core" % "3.12.0",
      "co.fs2" %% "fs2-io" % "3.12.0",
      "org.http4s" %% "http4s-ember-client" % "0.23.30",
      "org.http4s" %% "http4s-core" % "0.23.30"
    )
  ).dependsOn(desktopCatsDevKit)


// for running glfw in sbt shell
desktopCatsExample/run/fork := true
desktopCatsExample/run/javaOptions ++= Seq("-XstartOnFirstThread")
// end for sbt shell


lazy val desktopZioExample = (project in file("desktop/example/zio"))
  .settings(commonSettings("desktop-example-zio", s"$packagePrefix.desktop.example.zio"))
  .settings(
    libraryDependencies ++= catsLibs ++ zioLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List(
      "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6",
      "co.fs2" %% "fs2-core" % "3.12.0",
      "co.fs2" %% "fs2-io" % "3.12.0",
      "dev.zio" %% "zio-http" % "3.4.0"
    )
  )
  .dependsOn(widget, desktopSkijaBindings, layout, loops)
  .dependsOn(catnip, catnipZio, glfw, desktopWidgetLibrary, desktopZioDevKit)


// for running glfw in sbt shell
desktopZioExample/run/fork := true
desktopZioExample/run/javaOptions ++= Seq("-XstartOnFirstThread")
// end for sbt shell

