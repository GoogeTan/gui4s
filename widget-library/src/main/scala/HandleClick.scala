package me.katze.gui4s.widget.library

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.Point2d
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.EventCatcherWithRect

import scala.language.experimental.namedTypeArguments

type ClickHandler[Widget, Update, MouseClick] =
  Widget => ((Path, MouseClick) => Update) => Widget

def makeClickHandler[
  Widget,
  Update[_] : Monad,
  HandleableEvent,
  MeasurementUnit : Numeric,
  MouseClick
](
  eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Boolean], MeasurementUnit, HandleableEvent],
  currentMousePosition : Update[Point2d[MeasurementUnit]]
)(
  approprieteEvent: HandleableEvent => Option[MouseClick],
): ClickHandler[
  Widget,
  Update[Boolean],
  MouseClick
] =
  original => onClick =>
    eventCatcherWithRect(original):
      (path, widgetBoundingBox, event) =>
        currentMousePosition.flatMap:
          mousePosition =>
            approprieteEvent(event) match
              case Some(click) if widgetBoundingBox.containsPoint(mousePosition) =>
                onClick(path, click)
              case _ =>
                false.pure[Update]
            end match
end makeClickHandler