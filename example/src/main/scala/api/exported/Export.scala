package me.katze.gui4s.example
package api.exported

import api.{LayoutPlacementMeta, skijaWidgetsAreMergable, given}
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.data.StateT
import cats.{Functor, Monad}
import me.katze.gui4s.example.api.widget.skijaLinearLayout
import me.katze.gui4s.example.impl.containerPlacementCurried
import me.katze.gui4s.example.{*, given}
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import cats.syntax.all.*
import me.katze.gui4s.widget.{EventReaction, Path, given}

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

// TODO может, можно сделать более общим без таких уточнений
// TODO Remove using errors
def skijaRow[F[+_] : {Monad, FFI}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    SkijaUpdateT[Event],
    SkijaPlaceT[F],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F], PlacedWidget[F, *, DownEvent], Float](
      errors,
      getBounds,
      setBounds
    )(Axis.Horizontal, _, horizontalStrategy, verticalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    false.pure // TODO
  )
end skijaRow

def skijaColumn[F[+_] : {Monad, FFI}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout[
    SkijaUpdateT[Event],
    SkijaPlaceT[F],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F], PlacedWidget[F, *, DownEvent], Float](
      errors,
      getBounds,
      setBounds
    )(Axis.Horizontal, _, verticalStrategy, horizontalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    false.pure // TODO
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
  me.katze.gui4s.example.api.widget.skijaStateful[
    SkijaUpdate,
    SkijaPlaceT[F],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    EventReaction[State, Event, Nothing],
    State,
    Event,
    ChildEvent
  ](
    widgetsAreMergeable = skijaWidgetsAreMergable[
      Update = SkijaUpdateT[ChildEvent],
      SimplePlace = SkijaPlaceInner[F, *],
      InnerPlace = Sized[Float, *]
    ],
    runEventReaction = runEventReaction,
    typeCheckState = ???
  )(
    name = name,
    initialState = initialState, 
    handleEvent = handleEvent, 
    render = render,
    destructor = destructor
  )
end skijaStateful