package gui4s.core.widget.library.decorator

import cats._
import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.EventCatcherWithRect

/**
 * Виджет, позволяющий ловить событий мышки. Если событие произошло, пока мышь находится за пределами виджета,
 * то оно не будет обработано.
 */
def clickCatcher[
  Widget,
  Update[_] : Monad,
  EnvironmentalEvent,
  MouseClick,
  Point,
  WidgetBoundingBox
](
   eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Unit], WidgetBoundingBox],
   currentMousePosition : Update[Point],
   catchMouseEvent : Path => (MouseClick => Update[Boolean]) => Update[Unit],
   onClick : (Path, MouseClick) => Update[Boolean],
   isIn : Point => WidgetBoundingBox => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    (widgetBoundingBox, path) =>
      catchMouseEvent(path)(
        click =>
          currentMousePosition.flatMap:
            mousePosition => isIn(mousePosition)(widgetBoundingBox).ifM(
              ifTrue = onClick(path, click),
              ifFalse = false.pure[Update],
            )
      )
end clickCatcher