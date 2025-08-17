package me.katze.gui4s.example
package api.widget

import cats.{Functor, Monad}
import me.katze.gui4s.example.api.effects.{*, given}
import me.katze.gui4s.widget.library.Widget
import me.katze.gui4s.widget.library.decorator.{MapEvent, mapEvent}

def skijaMapEvent[
  IO[_] : Monad,
  MeasurementUnit,
  Clip,
  UpdateError,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : MapEvent[[Event] =>> Place[Widget[SkijaUpdateT[IO, MeasurementUnit, Clip, UpdateError, Event], Place, Draw, RecompositionReaction, HandleableEvent]]] =
  mapEvent([T, A, B] => (f : A => B) => SkijaUpdate.mapEvents(f))
end skijaMapEvent