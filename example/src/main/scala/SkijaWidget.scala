package me.katze.gui4s.example

import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.merge.MergesWithOldStates
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates

type SkijaWidget_[
  +Update[+_],
  +Place[+_],
  +Draw,
  RecompositionReaction,
  -HandleableEvent
] = SkijaWidget[?, Update, Place, Draw, RecompositionReaction, HandleableEvent]

final case class SkijaWidget[
  T,
  +Update[+_],
  +Place[+_],
  +Draw,
  RecompositionReaction,
  -HandleableEvent
](
   valueToDecorate : T,
   valueAsFree : AsFree[T, Place[T]],
   valueIsDrawable: Drawable[T, Draw],
   valueHandlesEvent: HandlesEvent[T, HandleableEvent, Update[Place[T]]],
   valueMergesWithOldState : MergesWithOldStates[T, RecompositionReaction, Place[T]],
   valueReactsOnRecomposition : ReactsOnRecomposition[T, RecompositionReaction],
   valueHasInnerState : HasInnerStates[T, RecompositionReaction],
):
  def withValue(newValue : T): SkijaWidget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
    copy(valueToDecorate = newValue)
  end withValue
end SkijaWidget

def skijaWidgetAsFree[
  Update[+_],
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : AsFree[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]]
] =
  self =>
    self.valueAsFree(self.valueToDecorate).map(self.withValue)
    
end skijaWidgetAsFree

def skijaWidgetIsDrawable[
  Update[+_],
  Place[+_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : Drawable[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Draw
] =
  self => self.valueIsDrawable(self.valueToDecorate)
end skijaWidgetIsDrawable

def skijaWidgetHandlesEvent[
  Update[+_] : Functor,
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : HandlesEvent[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  HandleableEvent,
  Update[Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]
] =
  (self, pathToParent, event) =>
    self.valueHandlesEvent(self.valueToDecorate, pathToParent, event)
      .map(_.map(self.withValue))
end skijaWidgetHandlesEvent

def skijaWidgetMergesWithOldState[
  Update[+_],
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : MergesWithOldStates[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction,
  Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
] =
  (self, path, oldState) =>
    self.valueMergesWithOldState(self.valueToDecorate, path, oldState)
      .map(self.withValue)

def skijaWidgetReactOnRecomposition[
  Update[+_],
  Place[+_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : ReactsOnRecomposition[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  (self, pathToParent, oldStates) =>
    self.valueReactsOnRecomposition(self.valueToDecorate, pathToParent, oldStates)
end skijaWidgetReactOnRecomposition

def skijaWidgetHasInnerStates[
Update[+_],
Place[+_],
Draw,
RecompositionReaction,
HandleableEvent
] : HasInnerStates[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  self =>
    self.valueHasInnerState(self.valueToDecorate)
end skijaWidgetHasInnerStates

