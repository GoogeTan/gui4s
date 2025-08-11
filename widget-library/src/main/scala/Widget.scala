package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.{Path, StateTree}
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, HandlesEventF}
import me.katze.gui4s.widget.library.Widget.ValueWrapper
import me.katze.gui4s.widget.merge.MergesWithOldStates
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates

@SuppressWarnings(Array("org.wartremover.warts.Any"))
type ValueType[W <: Widget[?, ?, ?, ?, ?]] = W match {
  case Widget.ValueWrapper[t, _, _, _, _, _] => t
}

enum Widget[
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
](
   val asFree : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
   val draw : Draw,
   val handleEvent : (path : Path, event : HandleableEvent) => Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
   val mergeWithOldState : (path: Path, oldState:  Map[String, StateTree[RecompositionReaction]]) => Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
   val reactOnRecomposition : (pathToParent: Path, oldStates: Map[String, StateTree[RecompositionReaction]]) => RecompositionReaction,
   val innerStates: Map[String, StateTree[RecompositionReaction]]
):
  case ValueWrapper[T, Update_[_], Place_[_], Draw_, RecompositionReaction_, HandleableEvent_](
    valueToDecorate : T,
    valueAsFree : AsFree[T, Place_[T]],
    valueIsDrawable: Drawable[T, Draw_],
    valueHandlesEvent: HandlesEvent[T, HandleableEvent_, Update_[Place_[T]]],
    valueMergesWithOldState : MergesWithOldStates[T, RecompositionReaction_, Place_[T]],
    valueReactsOnRecomposition : ReactsOnRecomposition[T, RecompositionReaction_],
    valueHasInnerState : HasInnerStates[T, RecompositionReaction_],
  )(using val updateFunctor : Functor[Update_], val placeFunctor : Functor[Place_]) extends Widget[Update_, Place_, Draw_, RecompositionReaction_, HandleableEvent_](
    valueAsFree(valueToDecorate).map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)),
    valueIsDrawable(valueToDecorate),
    (a, b) => valueHandlesEvent(valueToDecorate, a, b).map(_.map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState))),
    (a, b) => valueMergesWithOldState(valueToDecorate, a, b).map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)),
    valueReactsOnRecomposition(valueToDecorate, _, _),
    valueHasInnerState(valueToDecorate)
  )

  def asWrapper : Widget.ValueWrapper[?, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
    this match
      case wrapper : Widget.ValueWrapper[t, Update, Place, Draw, RecompositionReaction, HandleableEvent] => wrapper
    end match
  end asWrapper
end Widget

extension[T, Update[_] : Functor, Place[_] : Functor, Draw, RecompositionReaction, HandleableEvent](wrapper : ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent])
  def withValue(value: T): ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
    wrapper.copy(valueToDecorate = value)
  end withValue
end extension

def widgetAsFree[
  Update[_],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : AsFree[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]
] =
  _.asFree
end widgetAsFree

def widgetIsDrawable[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : Drawable[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  Draw
] =
  _.draw
end widgetIsDrawable

def widgetHandlesEvent[
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
] : HandlesEventF[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  HandleableEvent,
  Update * Place
] =
  _.handleEvent(_, _)
end widgetHandlesEvent


def widgetHandlesEventTyped[
  T,
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent
]: HandlesEventF[
  ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
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
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction,
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
] =
  _.mergeWithOldState(_, _)

def widgetReactsOnRecomposition[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : ReactsOnRecomposition[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  _.reactOnRecomposition(_, _)
end widgetReactsOnRecomposition

def widgetHasInnerStates[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
] : HasInnerStates[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  RecompositionReaction
] =
  _.innerStates
end widgetHasInnerStates

