package me.katze.gui4s.widget.library

import catnip.BiMonad
import cats.syntax.all.*
import cats.{Bifunctor, Bimonad, Functor}
import catnip.syntax.all.{*, given}
import me.katze.gui4s.widget.handle.mapEventHandle

type MapEvent[Widget[_]] = [A, B] => (value : Widget[A]) => (f : A => B) => Widget[B]

def mapEvent[
  Update[_, _] : BiMonad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  mapEventInUpdate : [T, A, B] => (A => B) => Update[A, T] => Update[B, T]
) : MapEvent[
  [Event] =>> Place[Widget[Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent]]
]  =
  [A, B] => value => f =>
    value.map {
      case widget: Widget.ValueWrapper[t, Update[A, *], Place, Draw, RecompositionReaction, HandleableEvent] =>
        widget.copy(
          valueHandlesEvent =
            mapEventHandle(widget.valueHandlesEvent)(mapEventInUpdate(f))
        )
    }
end mapEvent
