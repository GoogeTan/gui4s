package gui4s.desktop.windowing.glfw

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.{KeyAction, KeyModes}
import glfw4s.core.pure.{PureInput, PurePostInit}
import gui4s.core.geometry.Point2d
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.decorator.*


final case class ClickCatcherEvent(mousePosition : Point2d[Float])

def clickEventSource[
  Monitor,
  Window,
  Cursor,
  Joystick
](
  window: Window,
  glfw : PurePostInit[IO, IO[Unit], Monitor, Window, Cursor, Joystick],
  queue: Queue[IO, DownEvent]
) : IO[ClickEventSource] =
  glfw.addMouseButtonCallback(
    window,
    (window, button, action, mods) =>
      if action == KeyAction.Press then
        for
          monitor <- glfw.getPrimaryMonitor
          (scaleX, scaleY) <- glfw.getMonitorContentScale(monitor.get)
          (x, y) <- glfw.getCursorPos(window)
          mousePosition = Point2d(x.toFloat * scaleX, y.toFloat * scaleY)
          _ <- queue.offer(DownEvent.UserEvent(ClickCatcherEvent(mousePosition)))
        yield ()
      else
        IO.unit
  ).as({
    case DownEvent.UserEvent(ClickCatcherEvent(mousePosition)) =>
      Some(mousePosition)
    case _ => None
  })
end clickEventSource

