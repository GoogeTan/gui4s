package gui4s.desktop.windowing.glfw

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.pure.PureInput

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.trueUpdateDecoratorWithRect

final case class InnerEvent(event : ScrollEvent, childSize: Rect[Float])

def glfwScrollEventSource[
  Window,
  Cursor,
  Joystick
](
  glfw: PureInput[IO, IO[Unit], Window, Cursor, Joystick],
  window: Window,
  eventBus: Queue[IO, DownEvent]
): Init[ScrollEventSource[Point2d[Float]]] =
  Init.eval(
    glfw.addScrollCallback(
      window,
      (_, dx, dy) =>
        eventBus.offer(UserEvent(ScrollEvent(dx.toFloat, dy.toFloat)))
    )
  ).as(
    new ScrollEventSource[Point2d[Float]]:
      extension (event: Point2d[Float])
        override def shiftAlong(axis: Axis): Float =
          event.along(axis)
        end shiftAlong

        def withShiftAlong(axis: Axis, value : Float) : Point2d[Float] =
          event.withAlong(axis, value)
        end withShiftAlong
      end extension

      override def emitLeftovers[Event](leftovers: Point2d[Float]): Update[Event, Unit] =
        Update.emitEnvironmentalEvents(List(UserEvent(ScrollEvent(leftovers.x, leftovers.y))))
      end emitLeftovers

      def lower[Event](
        original: DesktopWidget[Event]
      ): DesktopWidget[Event] =
        trueUpdateDecoratorWithRect(
          original,
          situatedWidget =>
            Update.updateEnvironmentalEventsM_(events =>
              events.map {
                case UserEvent(event: ScrollEvent) =>
                  UserEvent(InnerEvent(event, situatedWidget.size))
                case event => event
              }.pure
            ) *> widgetHandlesEvent(situatedWidget.value)
        )

      override def eventSource[Event](
        original: ([Event2] => DesktopWidget[Event2] => DesktopWidget[Event2]) => DesktopWidget[Event],
        onScroll: (Situated[DesktopPlacedWidget[Event]], Point2d[Float], Rect[Float]) => Event,
      ): DesktopWidget[Event] =
        trueUpdateDecoratorWithRect(
          original([Event2] => body => lower(body)),
          situatedWidget =>
            widgetHandlesEvent(situatedWidget.value)
              <* Update.updateEnvironmentalEventsM_(events =>
              val (scrollEvents, notScrollEvents) = events.partitionMap {
                case UserEvent(event: InnerEvent) =>
                  Left((Point2d(event.event.dx, event.event.dy), event.childSize))
                case event =>
                  Right(event)
              }
              Update
                .emitEvents(scrollEvents.map(onScroll(situatedWidget, _, _)))
                .as(notScrollEvents)
            )
        )

  )
end glfwScrollEventSource
