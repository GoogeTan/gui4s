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
  isIn : Point => Shape => Boolean
) : Decorator[Widget] =
  eventCatcherWithRect:
    (path, widgetBoundingBox, event) =>
      currentMousePosition.flatMap:
        mousePosition =>
          approprieteEvent(event) match
            case Some(click) if isIn(mousePosition)(widgetBoundingBox) =>
              onClick(path, click)
            case _ =>
              false.pure[Update]
          end match
end clickCatcher