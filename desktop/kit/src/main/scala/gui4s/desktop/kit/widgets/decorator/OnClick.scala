package gui4s.desktop.kit
package widgets.decorator

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.KeyAction
import glfw4s.core.KeyModes
import glfw4s.core.pure.PurePostInit
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.RectAtPoint2d
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.clickCatcher as genericClickCatcher
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*

final case class MouseEvent(keyCode: Int, action : KeyAction, modes: KeyModes, mousePosition : Point2d[Float])

type ClickEventSource = DownEvent => Option[MouseEvent]

trait ClickCatcher:
  def apply[Event](eventOnClick: Event): Decorator[DesktopWidget[Event]]

  extension[Event](self: DesktopWidget[Event])
    def onClick(eventOnClick: Event) : DesktopWidget[Event] =
      apply(eventOnClick)(self)
    end onClick
  end extension
end ClickCatcher

def clickCatcher(
  extractEvent : ClickEventSource,
) : ClickCatcher =
  new ClickCatcher:
    override def apply[Event](eventOnClick: Event): Decorator[DesktopWidget[Event]] =
      genericClickCatcher[
        DesktopWidget[Event],
        UpdateC[Event],
        DownEvent,
        MouseEvent,
        Situated[DesktopPlacedWidget[Event]]
      ](
        eventCatcherWithRect = eventCatcher,
        catchMouseEvent = callback =>
          Update.handleEnvironmentalEvents_(event =>
            extractEvent(event).fold(
              false.pure[UpdateC[Event]]
            )(callback)
          ),
        onClick = {
          case MouseEvent(_, KeyAction.Release, _, _) => Update.emitEvents[Event](List(eventOnClick)).as(true)
          case _ => false.pure[UpdateC[Event]]
        },
        isMouseIn = (shape, event) =>
          Update.getCornerCoordinates.map(
            coordinatesOfTopLeftCornet =>
              RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY)
                .containsPoint(event.mousePosition)
          )
      )
end clickCatcher

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
    clickCatcher
  )
end clickCatcher

final case class ClickCatcherEvent(mouseEvent: MouseEvent)

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
      for
        monitor <- glfw.getPrimaryMonitor
        (scaleX, scaleY) <- glfw.getMonitorContentScale(monitor.get)
        (x, y) <- glfw.getCursorPos(window)
        mousePosition = Point2d(x.toFloat * scaleX, y.toFloat * scaleY)
        _ <- queue.offer(DownEvent.UserEvent(ClickCatcherEvent(MouseEvent(button, action, mods, mousePosition))))
      yield ()
  ).as({
    case DownEvent.UserEvent(ClickCatcherEvent(mouseEvent)) =>
      Some(mouseEvent) 
    case _ => None
  })
end clickEventSource
