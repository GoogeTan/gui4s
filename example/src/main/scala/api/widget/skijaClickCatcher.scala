package me.katze.gui4s.example
package api.widget

import api.effects.{SkijaUpdate, SkijaUpdateT}

import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.given
import me.katze.gui4s.geometry.{Point2d, RectAtPoint2d}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.widget.library.Widget
import me.katze.gui4s.widget.library.decorator.{Decorator, EventCatcherWithRect, clickCatcher}

def skijaClickCatcher[
  IO[_] : Monad,
  MeasurementUnit : Numeric,
  Clip,
  UpdateError,
  Place[_],
  Draw,
  RecompositionReaction,
  DownEvent,
  Event
](
  eventCatcher: EventCatcherWithRect[
    Place[Widget[SkijaUpdateT[IO, MeasurementUnit, Clip, UpdateError, Event], Place, Draw, RecompositionReaction, DownEvent]],
    SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Boolean],
    Sized[MeasurementUnit, Any],
    DownEvent
  ],
  extractMouseClickEvent : DownEvent => Option[Unit],
  mousePosition : IO[Point2d[MeasurementUnit]]
)(event : Event) : Decorator[Place[Widget[SkijaUpdateT[IO, MeasurementUnit, Clip, UpdateError, Event], Place, Draw, RecompositionReaction, DownEvent]]] =
  clickCatcher(
    eventCatcherWithRect = eventCatcher,
    currentMousePosition = SkijaUpdate.liftF(mousePosition),
    approprieteEvent = extractMouseClickEvent,
    onClick = (_, _) => SkijaUpdate.raiseEvents[IO, MeasurementUnit, Clip, UpdateError, Event](List(event)).as(true),
    isIn = point => shape =>
      SkijaUpdate.getCoordinates2d[IO, MeasurementUnit, Clip, UpdateError, Event].map(
        coordinatesOfTopLeftCornet =>
          RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet).containsPoint(point)
      )
  )
end skijaClickCatcher
