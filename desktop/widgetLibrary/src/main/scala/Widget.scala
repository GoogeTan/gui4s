package gui4s.decktop.widget.library

import catnip.syntax.additional.*
import cats.Functor
import cats.syntax.all.*
import gui4s.core.widget.{Path, StateTree}
import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.{HandlesEvent, HandlesEventF}
import gui4s.core.widget.merge.MergesWithOldStates
import gui4s.core.widget.recomposition.ReactsOnRecomposition
import gui4s.core.widget.state.HasInnerStates

type WidgetHandlesEvent[-HandleableEvent, +UpdatedWidget] = (pathToParent: Path, event: HandleableEvent) => UpdatedWidget

enum Widget[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent
](
  val asFree : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  val draw : Draw,
  val handleEvent : WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]],
  val mergeWithOldState : (path: Path, oldState:  Map[String, StateTree[RecompositionReaction]]) => Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  val reactOnRecomposition : (pathToParent: Path, oldStates: Map[String, StateTree[RecompositionReaction]]) => RecompositionReaction,
  val innerStates: Map[String, StateTree[RecompositionReaction]]
):
  case ValueWrapper[
    T, Update_[_] : Functor, Place_[_] : Functor, Draw_, RecompositionReaction_, HandleableEvent_
  ] private [library](// TODO remove private. Просто хочется удостовериться, что все корневые виджетьы создаются здесь
      valueToDecorate: T,
      valueAsFree: AsFree[T, Place_[T]],
      valueIsDrawable: Drawable[T, Draw_],
      valueHandlesEvent: HandlesEvent[T, HandleableEvent_, Update_[Place_[T]]],
      valueMergesWithOldState: MergesWithOldStates[T, RecompositionReaction_, Place_[T]],
      valueReactsOnRecomposition: ReactsOnRecomposition[T, RecompositionReaction_],
      valueHasInnerState: HasInnerStates[T, RecompositionReaction_],
    ) extends Widget[Update_, Place_, Draw_, RecompositionReaction_, HandleableEvent_](
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

