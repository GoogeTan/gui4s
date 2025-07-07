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
    children : List[Place[Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
    layout : Layout[Place, Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent], Meta],
    adjustDrawToMeta : (Draw, Meta) => Draw,
    adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
    eventConsumed : Update[Boolean],
) : Place[Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  type Widget = Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]
  layout(children).map(
    placedChildren =>
      Widget[
        Container[(Widget, Meta), Layout[Place, Widget, Meta]],
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
        valueHandlesEvent = containerHandlesEvent[Update, Place, Widget, HandleableEvent, Meta](
          childrenHandleEvent[Update, Place, Widget, HandleableEvent, Meta](
            widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            isEventConsumed = eventConsumed,
            adjustUpdateToMeta = adjustUpdateToMeta
          )
        ),
        valueMergesWithOldState = containerMergesWithOldStates[
          Place, Widget, RecompositionReaction, Meta
        ](
          widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]
        ),
        valueReactsOnRecomposition = containerReactsOnRecomposition[
          (Widget, Meta), Layout[Place, Widget, Meta], RecompositionReaction
        ](
          widgetWithMetaReactsOnRecomposition[Widget, Meta, RecompositionReaction](
            widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
          )
        ),
        valueHasInnerState = containerHasInnerStates[(Widget, Meta), Layout[Place, Widget, Meta], RecompositionReaction](
          widgetWithMetaHasInnerStates[Widget, Meta, RecompositionReaction](
            widgetHasInnerStates
          )
        ),
      )
  )
end linearLayout
