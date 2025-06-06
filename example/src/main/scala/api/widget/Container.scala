package me.katze.gui4s.example
package api.widget

import api.{*, given}
import impl.containerPlacementCurried2
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.syntax.all.given
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.*
import me.katze.gui4s.example.EventResult
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{Axis, MeasurableT, given}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.Container
import me.katze.gui4s.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import me.katze.gui4s.widget.free.containerAsFree
import me.katze.gui4s.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent}
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

def skijaLinearLayout[
  Update[+_] : Monad,
  Place[+_] : Functor,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  Meta : Ordering,
](
  children : List[Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
  layout : Layout[Place, SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent], Meta],
  adjustDrawToMeta : (Draw, Meta) => Draw,
  eventConsumed : Update[Boolean],
) : Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  type Widget = SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]
  layout(children).map(
    placedChildren =>
      SkijaWidget[
        Container[(Widget, Meta), Layout[Place, Widget, Meta]],
        Update, Place, Draw, RecompositionReaction, HandleableEvent
      ](
        valueToDecorate = Container(placedChildren, layout),
        valueAsFree = containerAsFree(
          skijaWidgetAsFree
        ),
        valueIsDrawable = drawContainer(
          widgetWithMetaIsDrawable(
            skijaWidgetIsDrawable,
            adjustDrawToMeta
          )
        ),
        valueHandlesEvent = containerHandlesEvent[Update, Place, Widget, HandleableEvent, Meta](
          childrenHandleEvent[Update, Place, Widget, HandleableEvent, Meta](
            widgetHandlesEvent = skijaWidgetHandlesEvent[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            widgetAsFree = skijaWidgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent],
            eventConsumed = eventConsumed
          )
        ),
        valueMergesWithOldState = containerMergesWithOldStates[
          Place, Widget, RecompositionReaction, Meta
        ](
          skijaWidgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]
        ),
        valueReactsOnRecomposition = containerReactsOnRecomposition[
          (Widget, Meta), Layout[Place, Widget, Meta], RecompositionReaction
        ](
          widgetWithMetaReactsOnRecomposition[Widget, Meta, RecompositionReaction](
            skijaWidgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
          )
        ),
        valueHasInnerState = containerHasInnerStates[(Widget, Meta), Layout[Place, Widget, Meta], RecompositionReaction](
          widgetWithMetaHasInnerStates[Widget, Meta, RecompositionReaction](
            skijaWidgetHasInnerStates
          )
        ),
      )
  )
end skijaLinearLayout
