package gui4s.core.widget.library.decorator

import cats.*
import cats.syntax.all.*
import gui4s.core.widget.library.decorator.{Decorator, EventCatcherWithRect}

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
   catchMouseEvent : (MouseClick => Update[Boolean]) => Update[Unit],
   onClick : MouseClick => Update[Boolean],
   isIn : Point => WidgetBoundingBox => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    widgetBoundingBox =>
      catchMouseEvent(
        click =>
          currentMousePosition.flatMap:
            mousePosition => isIn(mousePosition)(widgetBoundingBox).ifM(
              ifTrue = onClick(click),
              ifFalse = false.pure[Update],
            )
      )
end clickCatcher