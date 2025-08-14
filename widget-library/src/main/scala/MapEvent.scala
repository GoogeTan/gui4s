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
    value.map(
      placedWidget =>
        placedWidget.copy[
          Update[B, *],
          Place,
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
          handleEvent = (path, event) =>
              mapEventInUpdate(f)(placedWidget.handleEvent(path, event)).map(
                mapEvent(mapEventInUpdate)(_)(f)
              ),
          asFree = mapEvent(mapEventInUpdate)(placedWidget.asFree)(f),
          mergeWithOldState = (path, oldState) =>
            mapEvent(mapEventInUpdate)(placedWidget.mergeWithOldState(path, oldState))(f)
        )
    )
end mapEvent
