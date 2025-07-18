package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, HandlesEventF}
import me.katze.gui4s.widget.merge.MergesWithOldStates
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates

type Widget_[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  -HandleableEvent
] = Widget[?, Update, Place, Draw, RecompositionReaction, HandleableEvent]

final case class Widget[
  T,
  Update[_],
  Place[_],
  Draw,
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
  def withValue(newValue : T): Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
    copy(valueToDecorate = newValue)
  end withValue

  override def toString: String = 
    "W[" + valueToDecorate.toString + "]"
  end toString
end Widget

def widgetAsFree[
  Update[_],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : AsFree[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Place[Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]]
] =
  self =>
    self.valueAsFree(self.valueToDecorate).map(self.withValue)
end widgetAsFree

def widgetIsDrawable[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : Drawable[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Draw
] =
  self => self.valueIsDrawable(self.valueToDecorate)
end widgetIsDrawable

def widgetHandlesEvent[
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : HandlesEventF[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  HandleableEvent,
  Update * Place
] =
  (self, pathToParent, event) =>
    self.valueHandlesEvent(self.valueToDecorate, pathToParent, event)
      .map(_.map(self.withValue))
end widgetHandlesEvent


def widgetHandlesEventTyped[
  T,
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
]: HandlesEventF[
  Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
  HandleableEvent,
  Update * Place
] =
  (self, pathToParent, event) =>
    self.valueHandlesEvent(self.valueToDecorate, pathToParent, event)
      .map(_.map(self.withValue))
end widgetHandlesEventTyped

def widgetMergesWithOldState[
  Update[_],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : MergesWithOldStates[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction,
  Place[Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
] =
  (self, path, oldState) =>
    self.valueMergesWithOldState(self.valueToDecorate, path, oldState)
      .map(self.withValue)

def widgetReactsOnRecomposition[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : ReactsOnRecomposition[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  (self, pathToParent, oldStates) =>
    self.valueReactsOnRecomposition(self.valueToDecorate, pathToParent, oldStates)
end widgetReactsOnRecomposition

def widgetHasInnerStates[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : HasInnerStates[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  self =>
    self.valueHasInnerState(self.valueToDecorate)
end widgetHasInnerStates

