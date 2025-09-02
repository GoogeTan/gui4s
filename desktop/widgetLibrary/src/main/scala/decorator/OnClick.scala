package gui4s.desktop.widget.library
package decorator

import cats.*
import cats.syntax.all.*
import gui4s.core.widget.Path

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
  approprieteEvent: HandleableEvent => Option[MouseClick],
  onClick : (Path, MouseClick) => Update[Boolean],
  isIn : Point => WidgetBoundingBox => Update[Boolean]
) : Decorator[Widget] =
  eventCatcherWithRect:
    (path, widgetBoundingBox, event) =>
      currentMousePosition.flatMap:
        mousePosition =>
          isIn(mousePosition)(widgetBoundingBox).ifM(
            ifTrue = approprieteEvent(event) match
                case Some(click) =>
                  onClick(path, click)
                case _ =>
                  false.pure[Update],
            ifFalse = false.pure[Update],
          )
end clickCatcher