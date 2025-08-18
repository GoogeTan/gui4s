package me.katze.gui4s.example
package api.widget

import api.effects.{*, given}

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Applicative, Bimonad, Monad, Monoid}
import me.katze.gui4s.layout.{Sized, SizedT, given}
import me.katze.gui4s.widget.handle.HandlesEventF
import me.katze.gui4s.widget.library.{StatefulWidget, Widget, widgetsAreMergable}
import me.katze.gui4s.widget.{CatchEvents, Path, StatefulState, library}

import scala.reflect.Typeable

def skijaStateful[
  F[_] : Monad,
  Update[Event, Value] : {BiMonad, CatchEvents},
  Draw,
  RecompositionReaction: Monoid as RRM,
  HandleableEvent,
  PlaceError,
  MeasurementUnit,
](
    typecheckError: (Any, Path) => PlaceError,
) : StatefulWidget[
  [Event] =>> SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[Event, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]],
  Update,
  [Value] =>> Value => RecompositionReaction
] =
  new StatefulWidget[
    [Event] =>> SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[Event, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]],
    Update,
    [Value] =>> Value => RecompositionReaction
  ]:
    override def apply[
      State: Typeable,
      Event, 
      ChildEvent
    ](
      name: String,
      initialState: State,
      eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
      body: State => SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[ChildEvent, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]],
      destructor : State => RecompositionReaction
    ): SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[Event, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]] =
      library.stateful[
        Update,
        SkijaPlaceT[F, MeasurementUnit, PlaceError],
        Draw,
        RecompositionReaction,
        HandleableEvent,
        State,
        Event,
        ChildEvent
      ](
        widgetsAreMergeable = widgetsAreMergable[
          Update[ChildEvent, *],
          SkijaOuterPlaceT[F, MeasurementUnit, PlaceError],
          SizedT[MeasurementUnit],
          Draw,
          RecompositionReaction,
          HandleableEvent
        ],
        typeCheckState = [T] => (value: Any, path: Path, callback: StatefulState[State] => SkijaPlace[F, MeasurementUnit, PlaceError, T]) =>
          typecheckState[SkijaOuterPlaceT[F, MeasurementUnit, PlaceError], State](value, SkijaOuterPlace.raiseError(typecheckError(value, path)))
            .flatMap(callback)
      )(
        name = name,
        initialState = initialState,
        handleEvent = eventHandler,
        render = body,
        destructor = destructor
      ).map(a => a)
    end apply

    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
                                                            body: State => SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[ChildEvent, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]]
                                                          ): SkijaPlace[F, MeasurementUnit, PlaceError, Widget[Update[Event, *], SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]] =
      this(name, initialState, eventHandler, body, _ => RRM.empty)
    end apply
  end new
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
