package me.katze.gui4s.example
package api.exported

import api.{LayoutPlacementMeta, skijaWidgetsAreMergable, given}
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.{Functor, Monad}
import me.katze.gui4s.example.api.widget.skijaLinearLayout
import me.katze.gui4s.example.impl.containerPlacementCurried2
import me.katze.gui4s.example.given
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import cats.syntax.all.*
import me.katze.gui4s.widget.EventReaction
import scala.language.experimental.namedTypeArguments

// TODO может, можно сделать более общим без таких уточнений
// TODO Remove using errors
def skijaRow[F[+_] : {Monad, FFI}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    EventResult[*, Event],
    MeasurableT[F, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried2[F, PlacedWidget[F, *, DownEvent], Float](errors)(Axis.Horizontal, _, horizontalStrategy, verticalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    EventResult(false, Nil) // TODO
  )
end skijaRow

def skijaColumn[F[+_] : {Monad, FFI}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    EventResult[*, Event],
    MeasurableT[F, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried2[F, PlacedWidget[F, *, DownEvent], Float](errors)(Axis.Vertical, _, verticalStrategy, horizontalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    EventResult(false, Nil) // TODO
  )
end skijaColumn

def skijaStateful[
  F[_] : Monad,
  DownEvent,
  State,
  Event,
  ChildEvent
](
  name : String,
  initialState : State,
  handleEvent : (State, List[ChildEvent]) => EventReaction[State, Event, Nothing],
  render : State => Widget[F, ChildEvent, DownEvent],
  destructor : State => Recomposition[F],
) : Widget[F, Event, DownEvent] =
  given Functor[MeasurableT[F, Float]] = measurableIsFlatMap[F, Float]
  me.katze.gui4s.example.api.widget.skijaStateful[
    EventResult,
    MeasurableT[F, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    EventReaction[State, Event, Nothing],
    State,
    Event,
    ChildEvent
  ](
    widgetsAreMergeable = skijaWidgetsAreMergable[SimplePlace = PlacePartial[F, Float, *]], 
    runEventReaction = (reaction, _) => EventResult(reaction.newState, reaction.parentEvent), 
    typeCheckState = ???
  )(
    name = name,
    initialState = initialState, 
    handleEvent = handleEvent, 
    render = render,
    destructor = destructor
  )
end skijaStateful