package me.katze.gui4s.widget.library

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.Point2d
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.EventCatcherWithRect
import scala.language.experimental.namedTypeArguments

type ClickHandler[Widget, Update, MouseClick] =
  Widget => ((Path, MouseClick) => Update) => Widget

def makeClickHandler[
  Widget,
  Update[_] : Applicative,
  HandleableEvent,
  MeasurementUnit : Numeric,
  MouseClick
](
  eventCatcherWithRect: EventCatcherWithRect[Widget, Update[Boolean], MeasurementUnit, HandleableEvent],
  mouseTracker: WithContext[Widget, Point2d[MeasurementUnit]]
)(
  approprieteEvent: HandleableEvent => Option[MouseClick],
): ClickHandler[
  Widget,
  Update[Boolean],
  MouseClick
] =
  original => onClick =>
    mouseTracker:
      mousePosition =>
        eventCatcherWithRect(original):
          (path, widgetBoundingBox, event) =>
            approprieteEvent(event) match
              case Some(click) if widgetBoundingBox.containsPoint(mousePosition) =>
                onClick(path, click).as(true)
              case _ =>
                false.pure[Update]
            end match
end makeClickHandler