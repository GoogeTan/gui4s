package gui4s.desktop.kit
package widgets.decorator

import cats.effect.std.Queue
import cats.syntax.all.*
import cats.{Applicative, Monad}
import glfw4s.core.pure.PostInit
import glfw4s.core.{KeyAction, KeyModes}
import gui4s.core.geometry.{Point2d, RectAtPoint2d}
import gui4s.core.widget.library.decorator.{Decorator, clickCatcher as genericClickCatcher}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopWidget

final case class MouseEvent(keyCode: Int, action : KeyAction, modes: KeyModes)

type ClickEventSource = DownEvent => Option[MouseEvent]

def clickCatcher[IO[_] : Monad, Event](
                                        mousePosition : IO[Point2d[Float]],
                                        eventOnClick : Event,
                                        extractEvent : DownEvent => Option[MouseEvent]
                                      ) : Decorator[DesktopWidget[IO, Event]] =
  genericClickCatcher(
    eventCatcherWithRect = eventCatcher,
    currentMousePosition = Update.liftK[IO, Event](mousePosition),
    appropriateEvent = extractEvent,
    onClick = {
      case (_, MouseEvent(_, KeyAction.Release, _)) => Update.emitEvents(List(eventOnClick)).as(true)
      case _ => false.pure[UpdateC[IO,  Event]]
    },
    isIn = point => shape =>
      Update.getCornerCoordinates.map(
        coordinatesOfTopLeftCornet =>
          RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
      )
  )
end clickCatcher

final case class ClickCatcherEvent(mouseEvent: MouseEvent)

def clickEventSource[
  IO[_] : Applicative,
  CallbackEffect[_],
  Monitor,
  Window
](
  window: Window,
  glfw : PostInit[IO, CallbackEffect[Unit], Monitor, Window],
  queue: Queue[CallbackEffect, DownEvent]
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
