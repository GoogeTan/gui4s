package me.katze.gui4s.example
package api.exported

import api.exported.{*, given}

import catnip.syntax.all.{*, given}
import cats.{Applicative, Monad}
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.library.{StatefulWidget, widgetsAreMergable}
import me.katze.gui4s.widget.{EventReaction, Path, StatefulState}
import me.katze.gui4s.layout.given

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

def skijaStatefulWidget[IO[_]: Monad, UpdateError, PlaceError, MeasurementUnit, DownEvent](
  typecheckError: (Any, Path) => PlaceError,
): StatefulWidget[
  SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, *, DownEvent],
  Path => IO[Unit]
] =
  new StatefulWidget[
    SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, *, DownEvent],
    Path => IO[Unit]
  ]:
    override def apply[State : Typeable, Event, ChildEvent](
                                                              name: String,
                                                              initialState: State,
                                                              eventHandler: (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Path => IO[Unit]],
                                                              body: State => SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, ChildEvent, DownEvent]
                                                            ): SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =

        skijaStateful(
          name,
          initialState,
          eventHandler,
          body,
          (_ : State) => SkijaRecomposition.empty[IO],
          typecheckError
        )
    end apply
  end new
end skijaStatefulWidget

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
    SkijaDraw[F, OglGlfwWindow],
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
