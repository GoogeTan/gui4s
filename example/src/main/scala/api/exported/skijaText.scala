package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.additional.*
import catnip.syntax.all.given
import cats.{Applicative, Monad}
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.*
import me.katze.gui4s.glfw.GlfwWindow
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}
import me.katze.gui4s.widget.library.widgetsAreMergable
import me.katze.gui4s.widget.{EventReaction, Path, StatefulState}

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

def skijaText[
  IO[_] : Monad,
  UpdateError,
  PlaceError,
  MeasurementUnit,
  DownEvent,
  Event
](
  ffi : ForeighFunctionInterface[IO],
  textSizer : (String, SkijaTextStyle) => SkijaPlace[IO, MeasurementUnit, PlaceError, SkijaPlacedText],
  text : String,
  style : SkijaTextStyle,
) : SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.text[
    SkijaUpdateT[IO, UpdateError, MeasurementUnit, Event],
    SkijaPlaceT[IO, MeasurementUnit, PlaceError],
    SkijaDraw[IO, GlfwWindow[IO, Long, Float]],
    SkijaRecomposition[IO],
    DownEvent,
    SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    ().pure[IO],
  )
end skijaText

def skijaStateful[
  F[_] : Monad,
  UpdateError,
  PlaceError,
  MeasurementUnit,
  DownEvent,
  State : Typeable as ST,
  Event,
  ChildEvent,
](
    name : String,
    initialState : State,
    handleEvent : (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Path => F[Unit]],
    render : State => SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, ChildEvent, DownEvent],
    destructor : State => SkijaRecomposition[F],
    typecheckError : (Any, Path) => PlaceError
) : SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.stateful[
    SkijaUpdate[F, UpdateError, MeasurementUnit, *, *],
    SkijaPlaceT[F, MeasurementUnit, PlaceError],
    SkijaDraw[F, GlfwWindow[F, Long, Float]],
    SkijaRecomposition[F],
    DownEvent,
    EventReaction[State, Event, Path => F[Unit]],
    State,
    Event,
    ChildEvent
  ](
    widgetsAreMergeable = widgetsAreMergable[
      Update = SkijaUpdateT[F, UpdateError, MeasurementUnit, ChildEvent],
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
).map(a => a)
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
