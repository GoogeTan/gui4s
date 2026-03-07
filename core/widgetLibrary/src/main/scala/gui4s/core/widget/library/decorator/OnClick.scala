package gui4s.core.widget.library.decorator

import cats._
import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.EventCatcherWithRect

/**
 * Виджет, позволяющий ловить событий мышки. Если событие произошло, пока мышь находится за пределами виджета,
 * то оно не будет обработано.
 * @param eventCatcherWithRect
 * @param currentMousePosition
 * @param appropriateEvent
 * @param onClick
 * @param isIn
 * @tparam Widget
 * @tparam Update
 * @tparam EnvironmentalEvent
 * @tparam MouseClick
 * @tparam Point
 * @tparam WidgetBoundingBox
 * @return
 */
def clickCatcher[
  Widget,
  Update[_] : Monad,
  EnvironmentalEvent,
  MouseClick,
  Point,
  WidgetBoundingBox
](
   eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Boolean], WidgetBoundingBox, EnvironmentalEvent],
   currentMousePosition : Update[Point],
   appropriateEvent: EnvironmentalEvent => Option[MouseClick],
   onClick : (Path, MouseClick) => Update[Boolean],
   isIn : Point => WidgetBoundingBox => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    (path, widgetBoundingBox, event) =>
      appropriateEvent(event) match
        case Some(click) =>
          currentMousePosition.flatMap:
            mousePosition => isIn(mousePosition)(widgetBoundingBox).ifM(
              ifTrue =onClick(path, click),
            ifFalse = false.pure[Update],
          )
        case _ =>
          false.pure[Update]
end clickCatcher