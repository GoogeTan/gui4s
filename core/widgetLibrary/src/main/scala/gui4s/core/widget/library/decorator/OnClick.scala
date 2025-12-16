package gui4s.core.widget.library.decorator

import cats.*
import cats.syntax.all.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.{Decorator, EventCatcherWithRect}

def clickCatcher[
  Widget,
  Update[_] : Monad,
  HandleableEvent,
  MouseClick,
  Point,
  WidgetBoundingBox
](
   eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Boolean], WidgetBoundingBox, HandleableEvent],
   currentMousePosition : Update[Point],
   appropriateEvent: HandleableEvent => Option[MouseClick],
   onClick : (Path, MouseClick) => Update[Boolean],
   isIn : Point => WidgetBoundingBox => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    (path, widgetBoundingBox, event) =>
      currentMousePosition.flatMap:
        mousePosition =>
          isIn(mousePosition)(widgetBoundingBox).ifM(
            ifTrue = appropriateEvent(event) match
                case Some(click) =>
                  onClick(path, click)
                case _ =>
                  false.pure[Update],
            ifFalse = false.pure[Update],
          )
end clickCatcher