package gui4s.desktop.example.glfw

import cats.Monad
import cats.effect.IO
import cats.effect.std.Queue
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.PurePostInit
import glfw4s.jna.bindings.structs
import glfw4s.jna.bindings.types.{GLFWmonitor, GLFWwindow}
import gui4s.desktop.example.shared.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.windowing.glfw.*

object ScenedExample extends UIApp:
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
    Monad[Init].flatMap2(
        clickCatcher(window, glfw, eventBus),
        glfwScrollEventSource(glfw, window, eventBus)
    )(
      (clickSource, scrollSource) =>
        scenedExample(using clickSource, scrollSource, eventBus)
    )
  end main
end ScenedExample
