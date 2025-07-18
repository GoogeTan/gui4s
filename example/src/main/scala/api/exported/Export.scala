package me.katze.gui4s.example
package api.exported

import impl.containerPlacementCurried
import place.MainAxisStrategyErrors

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.data.{NonEmptyList, StateT}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad}
import me.katze.gui4s.example.{*, given}
import api.exported.given
import api.{LayoutPlacementMeta, given}

import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy, linearLayout, widgetsAreMergable, given}
import me.katze.gui4s.widget.{EventReaction, Path, StatefulState, given}

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

// TODO может, можно сделать более общим без таких уточнений
// TODO Remove using errors
def skijaRow[F[+_] : {Monad, ForeighFunctionInterface}, PlaceError, Event, DownEvent](using errors: MainAxisStrategyErrors[PlaceError])(
  children : List[SkijaWidget[F, Float, PlaceError, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): SkijaWidget[F, Float, PlaceError, Event, DownEvent] =
  linearLayout[
    SkijaUpdateT[F, Float, Event],
    SkijaPlaceT[F, Float, PlaceError],
    SkijaDraw[F, OglWindow],
    SkijaRecomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F, Float, PlaceError], SkijaPlacedWidget[F, Float, PlaceError, *, DownEvent], Float, PlaceError](
      errors,
      skijaGetBounds,
      skijaSetBounds,
    )(Axis.Horizontal, _, horizontalStrategy, verticalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    [T] => (update, meta) => addCoordinates(meta.point) *> update <* addCoordinates(-meta.point),
    false.pure[SkijaUpdateT[F, Float, Event]] // TODO
  )
end skijaRow

def skijaColumn[F[+_] : {Monad, ForeighFunctionInterface}, PlaceError, Event, DownEvent](using errors: MainAxisStrategyErrors[PlaceError])(
  children: List[SkijaWidget[F, Float, PlaceError, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): SkijaWidget[F, Float, PlaceError, Event, DownEvent] =
  linearLayout[
    SkijaUpdateT[F, Float, Event],
    SkijaPlaceT[F, Float, PlaceError],
    SkijaDraw[F, OglWindow],
    SkijaRecomposition[F],
    DownEvent,
    LayoutPlacementMeta[Float]
  ](
    children,
    containerPlacementCurried[SkijaPlaceInnerT[F, Float, PlaceError], SkijaPlacedWidget[F, Float, PlaceError, *, DownEvent], Float, PlaceError](
      errors,
      skijaGetBounds,
      skijaSetBounds,
    )(Axis.Vertical, _, verticalStrategy, horizontalStrategy),
    (effect, meta) => drawAt(summon, effect, meta.x, meta.y),
    [T] => (update, meta) => addCoordinates(meta.point) *> update <* addCoordinates(-meta.point),
    false.pure[SkijaUpdateT[F, Float, Event]] // TODO
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
   handleEvent : (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Nothing], // TODO Allow tasks
   render : State => SkijaWidget[F, MeasurementUnit, PlaceError, ChildEvent, DownEvent],
   destructor : State => SkijaRecomposition[F],
   typecheckError : (Any, Path) => PlaceError
) : SkijaWidget[F, MeasurementUnit, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.stateful[
    SkijaUpdate[F, MeasurementUnit, *, *],
    SkijaPlaceT[F, MeasurementUnit, PlaceError],
    SkijaDraw[F, OglWindow],
    SkijaRecomposition[F],
    DownEvent,
    EventReaction[State, Event, Nothing],
    State,
    Event,
    ChildEvent
  ](
    widgetsAreMergeable = widgetsAreMergable[
      Update = SkijaUpdateT[F, MeasurementUnit, ChildEvent],
      OuterPlace = SkijaPlaceInnerT[F, MeasurementUnit, PlaceError],
    ],
    runEventReaction = runEventReaction,
    typeCheckState = [T] => (value : Any, path : Path, callback : StatefulState[State] => SkijaPlace[F, MeasurementUnit, PlaceError, T]) =>
      typecheckState[SkijaPlaceInnerT[F, MeasurementUnit, PlaceError], State](value, raiseError(typecheckError(value, path)))
        .flatMap(callback)
  )(
    name = name,
    initialState = initialState, 
    handleEvent = handleEvent, 
    render = render,
    destructor = destructor
  )
end skijaStateful

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def typecheckState[F[_] : Applicative, S: Typeable as ST](any: Any, raiseError : F[Nothing]): F[StatefulState[S]] =
  any match
    case StatefulState(a : S, b : S) =>
      StatefulState(a, b).pure[F]
    case _ =>
      raiseError.map(a => a)
  end match
end typecheckState