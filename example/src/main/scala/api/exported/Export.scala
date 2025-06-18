package me.katze.gui4s.example
package api.exported

import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy, linearLayout, skijaWidgetsAreMergable, given}
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.data.StateT
import cats.{Applicative, Functor, Monad}
import me.katze.gui4s.example.impl.containerPlacementCurried
import me.katze.gui4s.example.{*, given}
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import cats.syntax.all.*
import me.katze.gui4s.example.api.{LayoutPlacementMeta, given}
import me.katze.gui4s.widget.{EventReaction, Path, given}
import me.katze.gui4s.example.api.exported.given 

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

// TODO может, можно сделать более общим без таких уточнений
// TODO Remove using errors
def skijaRow[F[+_] : {Monad, FFI}, PlaceError, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Float, PlaceError, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Float, PlaceError, Event, DownEvent] =
  linearLayout[
    SkijaUpdateT[Event],
    SkijaPlaceT[F, PlaceError, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F, PlaceError, Float], PlacedWidget[F, Float, PlaceError, *, DownEvent], Float](
      errors,
      skijaGetBounds,
      skijaSetBounds,
    )(Axis.Horizontal, _, horizontalStrategy, verticalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    false.pure // TODO
  )
end skijaRow

def skijaColumn[F[+_] : {Monad, FFI}, PlaceError, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Float, PlaceError, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Float, PlaceError, Event, DownEvent] =
  linearLayout[
    SkijaUpdateT[Event],
    SkijaPlaceT[F, PlaceError, Float],
    SkijaDraw[F, OglWindow],
    Recomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F, PlaceError, Float], PlacedWidget[F, Float, PlaceError, *, DownEvent], Float](
      errors,
      skijaGetBounds,
      skijaSetBounds,
    )(Axis.Horizontal, _, verticalStrategy, horizontalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    false.pure // TODO
  )
end skijaColumn

def skijaStateful[
  F[_] : Monad,
  PlaceError,
  MeasurementUnit,
  DownEvent,
  State : Typeable as ST,
  Event,
  ChildEvent
](
    name : String,
    initialState : State,
    handleEvent : (State, List[ChildEvent]) => EventReaction[State, Event, Nothing], // TODO Allow tasks
    render : State => Widget[F, MeasurementUnit, PlaceError, ChildEvent, DownEvent],
    destructor : State => Recomposition[F],
    typecheckError : Any => PlaceError
) : Widget[F, MeasurementUnit, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.skijaStateful[
    SkijaUpdate,
    SkijaPlaceT[F, PlaceError, MeasurementUnit],
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
      SimplePlace = SkijaPlaceInner[F, PlaceError, MeasurementUnit, *],
      InnerPlace = Sized[MeasurementUnit, *]
    ],
    runEventReaction = runEventReaction,
    typeCheckState = value => 
      typecheckState[SkijaPlaceT[F, PlaceError, MeasurementUnit], PlaceError](value, raiseError[F, PlaceError, MeasurementUnit](typecheckError(value)))

  )(
    name = name,
    initialState = initialState, 
    handleEvent = handleEvent, 
    render = render,
    destructor = destructor
  )
end skijaStateful

def typecheckState[F[+_] : Applicative, S: Typeable](any: Any, raiseError : F[Nothing]): F[(S, S)] =
  any match
    case (a : S, b : S) => (a, b).pure[F]
    case None => raiseError
  end match
end typecheckState