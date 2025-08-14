package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.{Path, StateTree}
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, HandlesEventF}
import me.katze.gui4s.widget.merge.MergesWithOldStates
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates

type WidgetHandlesEvent[-HandleableEvent, +UpdatedWidget] = (pathToParent: Path, event: HandleableEvent) => UpdatedWidget

final case class Widget[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
](
  asFree : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  draw : Draw,
  handleEvent : WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]],
  mergeWithOldState : (path: Path, oldState:  Map[String, StateTree[RecompositionReaction]]) => Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  reactOnRecomposition : (pathToParent: Path, oldStates: Map[String, StateTree[RecompositionReaction]]) => RecompositionReaction,
  innerStates: Map[String, StateTree[RecompositionReaction]]
)

object Widget:
  def ValueWrapper[
    T,
    Update[_] : Functor,
    Place[_] : Functor,
    Draw,
    RecompositionReaction,
    HandleableEvent
  ](
      valueToDecorate: T,
      valueAsFree: AsFree[T, Place[T]],
      valueIsDrawable: Drawable[T, Draw],
      valueHandlesEvent: HandlesEvent[T, HandleableEvent, Update[Place[T]]],
      valueMergesWithOldState: MergesWithOldStates[T, RecompositionReaction, Place[T]],
      valueReactsOnRecomposition: ReactsOnRecomposition[T, RecompositionReaction],
      valueHasInnerState: HasInnerStates[T, RecompositionReaction],
    ) : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent] = Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent](
    valueAsFree(valueToDecorate).map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)),
    valueIsDrawable(valueToDecorate),
    (a, b) => valueHandlesEvent(valueToDecorate, a, b).map(_.map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState))),
    (a, b) => valueMergesWithOldState(valueToDecorate, a, b).map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)),
    valueReactsOnRecomposition(valueToDecorate, _, _),
    valueHasInnerState(valueToDecorate)
  )
end Widget

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

