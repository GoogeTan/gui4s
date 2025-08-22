package me.katze.gui4s.widget.library
package decorator

import catnip.syntax.all.given
import cats.syntax.all.*
import cats.{Functor, Monad, ~>}

type MapEvent[Widget[_]] = [A, B] => (A => B) => Widget[A] => Widget[B] 

def mapUpdate[
  OldUpdate[_] : Monad,
  NewUpdate[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  original : Place[Widget[OldUpdate, Place, Draw, RecompositionReaction, HandleableEvent]],
  mapEventInUpdate : OldUpdate ~> NewUpdate
) :
  Place[Widget[NewUpdate, Place, Draw, RecompositionReaction, HandleableEvent]] =
  original.map(
    placedWidget =>
      final case class MapEvent(currentWidget: Widget[OldUpdate, Place, Draw, RecompositionReaction, HandleableEvent])
      Widget.ValueWrapper(
        valueToDecorate = MapEvent(placedWidget),
        valueAsFree = placed => placed.currentWidget.asFree.map(MapEvent(_)),
        valueIsDrawable = _.currentWidget.draw,
        valueHandlesEvent = (self, path, event) =>
          mapEventInUpdate(self.currentWidget.handleEvent(path, event)).map(
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
end mapUpdate
