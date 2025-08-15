package me.katze.gui4s.widget.library
package decorator

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Bifunctor, Bimonad, Functor}
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
        final case class MapEvent(currentWidget: Widget[Update[A, *], Place, Draw, RecompositionReaction, HandleableEvent])
        Widget.ValueWrapper(
          valueToDecorate = MapEvent(placedWidget),
          valueAsFree = placed => placed.currentWidget.asFree.map(MapEvent(_)),
          valueIsDrawable = _.currentWidget.draw,
          valueHandlesEvent = (self, path, event) =>
            mapEventInUpdate(f)(self.currentWidget.handleEvent(path, event)).map(
              _.map(MapEvent(_))
            ),
          valueMergesWithOldState = (self, path, states) =>
            self.currentWidget.mergeWithOldState(path, states).map(MapEvent(_)),
          valueReactsOnRecomposition = (self, path, states) =>
            self.currentWidget.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.currentWidget.innerStates
        )
    )
end mapEvent
