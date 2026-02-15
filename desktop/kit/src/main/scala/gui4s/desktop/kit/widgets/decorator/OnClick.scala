package gui4s.desktop.kit
package widgets.decorator

import cats.effect.std.Queue
import cats.effect.*
import cats.syntax.all._
import glfw4s.core.KeyAction
import glfw4s.core.KeyModes
import glfw4s.core.pure.PureInput

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.RectAtPoint2d
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.{clickCatcher => genericClickCatcher}

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget

final case class MouseEvent(keyCode: Int, action : KeyAction, modes: KeyModes)

type ClickEventSource = DownEvent => Option[MouseEvent]

trait ClickCatcher:
  def apply[Event](eventOnClick: Event): Decorator[DesktopWidget[Event]]

  extension[Event](self: DesktopWidget[Event])
    def onClick(eventOnClick: Event) : DesktopWidget[Event] =
      apply(eventOnClick)(self)
    end onClick
  end extension
end ClickCatcher

def clickCatcher[
  Window,
  Cursor,
  Joystick
](
  window: Window,
  glfw : PureInput[IO, IO[Unit], Window, Cursor, Joystick],
  queue: Queue[IO, DownEvent],
) : Init[ClickCatcher] =
  Init.eval(
    clickEventSource(window, glfw, queue)
  ).map(source =>
    new ClickCatcher:
      override def apply[Event](
                                        eventOnClick: Event,
                                      ): Decorator[DesktopWidget[Event]] =
        genericClickCatcher(
          eventCatcherWithRect = eventCatcher,
          currentMousePosition = Update.liftK[IO, Event](
            glfw.getCursorPos(window).map((x, y) => Point2d(x.toFloat, y.toFloat))
          ),
          appropriateEvent = source,
          onClick = {
            case (_, MouseEvent(_, KeyAction.Release, _)) => Update.emitEvents[IO, Event](List(eventOnClick)).as(true)
            case _ => false.pure[UpdateC[IO,  Event]]
          },
          isIn = point => shape =>
            Update.getCornerCoordinates.map(
              coordinatesOfTopLeftCornet =>
                RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
            )
        )
  )
end clickCatcher

final case class ClickCatcherEvent(mouseEvent: MouseEvent)

def clickEventSource[
  Window,
  Cursor,
  Joystick
](
  window: Window,
  glfw : PureInput[IO, IO[Unit], Window, Cursor, Joystick],
  queue: Queue[IO, DownEvent]
) : IO[ClickEventSource] =
  glfw.addMouseButtonCallback(
    window,
    (window, button, action, mods) =>
      queue.offer(DownEvent.UserEvent(ClickCatcherEvent(MouseEvent(button, action, mods))))
  ).as({
    case DownEvent.UserEvent(ClickCatcherEvent(mouseEvent)) =>
      Some(mouseEvent) 
    case _ => None
  })
end clickEventSource
