package me.katze.gui4s.example
package draw.skija

import api.{*, given}
import draw.skija.given
import impl.{LayoutPlacement, containerPlacementCurried2}
import place.MainAxisStrategyErrors

import catnip.syntax.all.given
import cats.{Functor, Monad, Monoid}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint}
import me.*
import me.katze.gui4s.example.EventResult
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.{Axis, Measurable, MeasurableT, given}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import me.katze.gui4s.widget.free.{AsFree, containerAsFree, widgetWithMetaAsFree}
import me.katze.gui4s.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent, handlesNothing}
import me.katze.gui4s.widget.merge.{anyHasNothingToMerge, containerMergesWithOldStates}
import me.katze.gui4s.widget.recomposition.{containerReactsOnRecomposition, hasNoReactionOnRecomposition, widgetWithMetaReactsOnRecomposition}
import me.katze.gui4s.widget.state.{containerHasInnerStates, hasNoInnerState, widgetWithMetaHasInnerStates}
import me.katze.gui4s.widget.{Container, given}

type Update[UpEvent] = [Value] =>> EventResult[Value, UpEvent]
type Recomposition[F[_]] = F[Unit]



type PlacedWidget[F[+_], +Event, -DownEvent] =  SkijaWidget_[[Value] =>> EventResult[Value, Event],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], +Event, -DownEvent] = Measurable[F, Float, PlacedWidget[F, Event, DownEvent]]

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

def skijaText[F[+_] : {Monad, Impure}, Window](using backend: SkijaBackend[F, Window])(text : String, style : SkijaTextStyle) : Widget[F, Nothing, Any] =
  skijaText[
    [Value] =>> EventResult[Value, Nothing],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], Any, SkijaPlacedText
  ](
    sizeText(text, backend.globalShaper, style),
    drawText,
    ().pure[F]
  )
end skijaText

def skijaText[
  Update[+_] : Monad,
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  PlacedText
](
  text : Place[PlacedText],
  draw : PlacedText => Draw,
  emptyRecomposition : RecompositionReaction
) : Place[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction, HandleableEvent](text.map(draw), emptyRecomposition)
end skijaText

def drawOnlyWidget[
  Update[+_] : Monad as M,
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](toDraw : Place[Draw], emptyRecomposition : RecompositionReaction) : Place[SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  toDraw.map(
    draw =>
      val asFree : AsFree[Draw, Place[Draw]] = (_ : Draw) => toDraw
      SkijaWidget[Draw, Update, Place, Draw, RecompositionReaction, HandleableEvent](
        valueToDecorate = draw,
        valueAsFree = asFree,
        valueIsDrawable = _ => draw,
        valueHandlesEvent = handlesNothing[Draw, HandleableEvent, Update[Place[Draw]]](asFree andThen M.pure),
        valueMergesWithOldState = anyHasNothingToMerge(asFree),
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[Draw, RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Draw]
      )
  )
end drawOnlyWidget

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
            skijaWidgetReactOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
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

// TODO Remove using errors
def skijaRow[F[+_] : {Monad, Impure}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    [Value] =>> EventResult[Value, Event],  
    MeasurableT[F, Float],
    SkijaDraw[F, OglWindow], 
    Recomposition[F], 
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried2[F, [Event] =>> PlacedWidget[F, Event, DownEvent], Float](errors)(Axis.Horizontal, _, horizontalStrategy, verticalStrategy),
    drawAt,
    false.pure[[Value] =>> EventResult[Value, Nothing]] // TODO
  )
end skijaRow

def skijaColumn[F[+_] : {Monad, Impure}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    [Value] =>> EventResult[Value, Event],
    MeasurableT[F, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried2[F, [Event] =>> PlacedWidget[F, Event, DownEvent], Float](errors)(Axis.Vertical, _, verticalStrategy, horizontalStrategy),
    drawAt,
    false.pure[[Value] =>> EventResult[Value, Nothing]] // TODO
  )
end skijaColumn

