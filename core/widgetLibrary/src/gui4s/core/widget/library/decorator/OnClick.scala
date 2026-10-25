package gui4s.core.widget.library.decorator

import cats.*
import cats.syntax.all.*

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
  WidgetBoundingBox
](
   eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Unit], WidgetBoundingBox],
   catchMouseEvent : (MouseClick => Update[Boolean]) => Update[Unit],
   onClick : MouseClick => Update[Boolean],
   isMouseIn : (WidgetBoundingBox, MouseClick) => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    widgetBoundingBox =>
      catchMouseEvent(
        click =>
          isMouseIn(widgetBoundingBox, click).ifM(
            ifTrue = onClick(click),
            ifFalse = false.pure[Update],
          )
      )
end clickCatcher