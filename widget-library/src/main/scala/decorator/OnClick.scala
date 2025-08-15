package me.katze.gui4s.widget.library
package decorator

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.Point2d
import me.katze.gui4s.widget.Path

import scala.language.experimental.namedTypeArguments

def clickCatcher[
  Widget,
  Update[_] : Monad,
  HandleableEvent,
  MouseClick,
  Point,
  Shape
](
  eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Boolean], Shape, HandleableEvent],
  currentMousePosition : Update[Point],
  approprieteEvent: HandleableEvent => Option[MouseClick],
  onClick : (Path, MouseClick) => Update[Boolean],
  isIn : Point => Shape => Update[Boolean]
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