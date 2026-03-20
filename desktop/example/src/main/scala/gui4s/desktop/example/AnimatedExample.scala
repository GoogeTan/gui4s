package gui4s.desktop.example

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all._
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.PurePostInit
import glfw4s.jna.bindings.structs
import glfw4s.jna.bindings.types.GLFWmonitor
import glfw4s.jna.bindings.types.GLFWwindow

import gui4s.desktop.kit.UIApp
import gui4s.desktop.kit.effects.DownEvent
import gui4s.desktop.kit.effects.Init
import gui4s.desktop.kit.widgets._
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.skija.typeface.defaultTypeface

object AnimatedExample extends UIApp:
  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s animation example",
    width = 620,
    height = 480,
  )

  override def main(
    glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, structs.GLFWcursor, Int],
    window: GLFWwindow,
    eventBus: Queue[IO, DownEvent]
  ): Init[DesktopWidget[Nothing]] =
    for
      onClick  <- clickCatcher(window, glfw, eventBus)
      textWidget <- TextWidget()
      resourceWidget <- ResourceWidget(eventBus)
      initializationWidget = InitializationWidget(resourceWidget)
      animation <- animationWidget[Float](eventBus)
      text <- TextWidget()
      typeface <- Init.evalResource(defaultTypeface[IO])
      scroll <- scrollWidget(glfw, window, eventBus, animation)
    yield
      animationExample(initializationWidget, onClick, animation, text, typeface)(using scroll)
  end main
end AnimatedExample
