package gui4s.desktop.windowing.glfw

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.pure.PureInput

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*

def scrollWidget[
  Window,
  Cursor,
  Joystick,
  Event
](
  glfw: PureInput[IO, IO[Unit], Window, Cursor, Joystick],
  window: Window,
  eventBus: Queue[IO, DownEvent]
) : Init[ScrollWidget] =
  glfwScrollEventSource(glfw, window, eventBus).map(
    gui4s.desktop.kit.widgets.scrollWidget(
      _,
      animationWidget
    )
  )
end scrollWidget