package gui4s.desktop.windowing.glfw

import cats.syntax.all.*
import cats.effect.IO
import cats.effect.std.Queue
import glfw4s.core.pure.PurePostInit
import gui4s.desktop.kit.effects.{*, given}
import gui4s.desktop.kit.widgets.decorator.ClickCatcher

def clickCatcher[
  Monitor,
  Window,
  Cursor,
  Joystick
](
  window: Window,
  glfw : PurePostInit[IO, IO[Unit], Monitor, Window, Cursor, Joystick],
  queue: Queue[IO, DownEvent],
) : Init[ClickCatcher] =
  Init.eval(clickEventSource(window, glfw, queue)).map(
    gui4s.desktop.kit.widgets.decorator.clickCatcher
  )
end clickCatcher