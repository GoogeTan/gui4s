package me.katze.gui4s.widget.library

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, Monoid}
import me.*
import me.katze.gui4s.widget.Container
import me.katze.gui4s.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import me.katze.gui4s.widget.free.containerAsFree
import me.katze.gui4s.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}
import me.katze.gui4s.widget.merge.containerMergesWithOldStates
import me.katze.gui4s.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import me.katze.gui4s.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

type LinearLayout[
  Widget[_],
  MeasurementUnit,
  Axis,
] = [Event] => (
  children               : List[Widget[Event]],
  mainAxis               : Axis,
  mainAxisStrategy       : MainAxisPlacementStrategy[MeasurementUnit],
  additionalAxisStrategy : AdditionalAxisPlacementStrategy,
) => Widget[Event]

def linearLayout[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  Meta : Ordering,
](
   children : List[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
   layout : Layout[Place, Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent], Meta],
   adjustDrawToMeta : (Draw, Meta) => Draw,
   adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
   isEventConsumed : Update[Boolean],
) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  type Widget_ = Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]
  layout(children).map(
    placedChildren =>
      Widget.ValueWrapper[
        Container[(Widget_, Meta), Layout[Place, Widget_, Meta]],
        Update, Place, Draw, RecompositionReaction, HandleableEvent
      ](
        valueToDecorate = Container(placedChildren, layout),
        valueAsFree = containerAsFree(
          widgetAsFree
        ),
        valueIsDrawable = drawContainer(
          widgetWithMetaIsDrawable(
            widgetIsDrawable,
            adjustDrawToMeta
          )
        ),
        valueHandlesEvent = containerHandlesEvent[Update, Place, Widget_, HandleableEvent, Meta](
          childrenHandleEvent[Update, Place, Widget_, HandleableEvent, Meta](
            widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            isEventConsumed = isEventConsumed,
            adjustUpdateToMeta = adjustUpdateToMeta
          )
        ),
        valueMergesWithOldState = containerMergesWithOldStates[
          Place, Widget_, RecompositionReaction, Meta
        ](
          widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]
        ),
        valueReactsOnRecomposition = containerReactsOnRecomposition[
          (Widget_, Meta), Layout[Place, Widget_, Meta], RecompositionReaction
        ](
          widgetWithMetaReactsOnRecomposition[Widget_, Meta, RecompositionReaction](
            widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
          )
        ),
        valueHasInnerState = containerHasInnerStates[(Widget_, Meta), Layout[Place, Widget_, Meta], RecompositionReaction](
          widgetWithMetaHasInnerStates[Widget_, Meta, RecompositionReaction](
            widgetHasInnerStates
          )
        ),
      )
  )
end linearLayout
