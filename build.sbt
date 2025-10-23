import sbt.Keys.libraryDependencies
import sbtide.Keys.idePackagePrefix

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

// for nix os
ThisBuild / javaOptions ++= Seq("-XstartOnFirstThread")
ThisBuild / javaOptions ++= sys.env.get("LD_LIBRARY_PATH").map(p => Seq(s"-Djava.library.path=$p", s"-Djna.library.path=$p")).getOrElse(Seq())
// end for nix os

Global / excludeLintKeys += idePackagePrefix

lazy val catsLibs = List("org.typelevel" %% "cats-core" % "2.13.0")
lazy val catsEffectLibs = catsLibs ++ List("org.typelevel" %% "cats-effect" % "3.6.3")
lazy val fs2Libs = List("co.fs2" %% "fs2-core" % "3.12.2")
lazy val zioLibs = List(
  "dev.zio" %% "zio" % "2.1.22",
  "dev.zio" %% "zio-interop-cats" % "23.1.0.5"
)
lazy val testLibs = List(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test",
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

lazy val skijaLibs = List(
  "io.github.humbleui" % "skija-shared" % "0.116.8",
  "io.github.humbleui" % s"skija-$os-$arch" % "0.116.4",
)

ThisBuild / resolvers += "gitverse" at "https://gitverse.ru/api/packages/daniilzemskov/maven"

val glfw4sVersion = "0.1.19"

lazy val glfwLibs = List(
  "glfw4s" % "core_3" % glfw4sVersion
)

def commonSettings(nameIn : String, pkg : String) =
  List(
     name := nameIn,
     idePackagePrefix := Some(pkg),
     wartremoverErrors := Warts.allBut(Wart.ListAppend, Wart.Overloading, Wart.Nothing, Wart.DefaultArguments),
     scalacOptions ++= List("-Wunused:imports", "-Xkind-projector")
  )

val packagePrefix = "gui4s"

lazy val geometry = (project in file("core/geometry"))
  .settings(commonSettings("geometry", packagePrefix + ".core.geometry"))
  .settings(libraryDependencies ++= catsLibs ++ testLibs)

lazy val catnip = (project in file("catnip"))
  .settings(commonSettings("catnip", "catnip"))
  .settings(libraryDependencies ++= catsEffectLibs ++ testLibs)

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

lazy val desktopSkijaBindings = (project in file("desktop/skija"))
  .settings(commonSettings("desktop-skija-bindings", s"$packagePrefix.desktop.skija"))
  .settings(
    libraryDependencies ++= catsEffectLibs ++ testLibs ++ skijaLibs ++ glfwLibs,
    fork := true,
  )
  .dependsOn(catnip, layout)

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
  .settings(libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs ++ glfwLibs)
  .dependsOn(widget, desktopSkijaBindings, layout, loops, catnip, desktopWidgetLibrary, commonDevKit)

lazy val desktopZioDevKit = (project in file("desktop/kit/zio"))
  .settings(commonSettings("desktop-kit-zio", s"$packagePrefix.desktop.kit.zio"))
  .settings(
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs ++ zioLibs,
    libraryDependencies += "glfw4s" % "zio_3" % glfw4sVersion,
    wartremoverErrors := Warts.allBut(Wart.ListAppend, Wart.Overloading, Wart.Nothing, Wart.DefaultArguments, Wart.Any)
  ).dependsOn(desktopCommonDevKit, catnipZio)

lazy val desktopCatsDevKit = (project in file("desktop/kit/cats"))
  .settings(commonSettings("desktop-kit-cats", s"$packagePrefix.desktop.kit.cats"))
  .settings(
    libraryDependencies ++= catsEffectLibs ++ fs2Libs ++ testLibs,
    libraryDependencies += "glfw4s" % "cats_3" % glfw4sVersion,
  )
  .dependsOn(desktopCommonDevKit, catnip)

lazy val desktopCatsExample = (project in file("desktop/example/cats"))
  .settings(commonSettings("desktop-example-cats", s"$packagePrefix.desktop.example.cats"))
  .settings(
    libraryDependencies ++= catsLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List(
      "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6",
      "co.fs2" %% "fs2-io" % "3.12.2",
      "org.http4s" %% "http4s-ember-client" % "0.23.32",
      "org.http4s" %% "http4s-core" % "0.23.32"
    ),
    fork := true,
    javaOptions ++= Seq("-XstartOnFirstThread")
  ).dependsOn(desktopCatsDevKit)


lazy val desktopZioExample = (project in file("desktop/example/zio"))
  .settings(commonSettings("desktop-example-zio", s"$packagePrefix.desktop.example.zio"))
  .settings(
    libraryDependencies ++= catsLibs ++ zioLibs ++ fs2Libs ++ testLibs ++ skijaLibs ++ List(
      "com.github.cb372" %% "scalacache-core" % "1.0.0-M6",
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6",
      "co.fs2" %% "fs2-core" % "3.12.2",
      "co.fs2" %% "fs2-io" % "3.12.2",
      "dev.zio" %% "zio-http" % "3.5.1"
    ),
    fork := true,
    javaOptions ++= Seq("-XstartOnFirstThread")
  )
  .dependsOn(widget, desktopSkijaBindings, layout, loops)
  .dependsOn(catnip, catnipZio, desktopWidgetLibrary, desktopZioDevKit)



