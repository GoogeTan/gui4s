package me.katze.gui4s.example
package api.effects

import catnip.BiMonad
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import catnip.transformer.instances.given
import cats.*
import cats.effect.ExitCode
import cats.kernel.Monoid
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Point2d, Point3d}
import me.katze.gui4s.widget.library.effect.EventsTransformer

type SkijaUpdateTransformer[UpdateError, State, Events] =
    StateTransformer[State] <> EventsTransformer[Events] <> ErrorTransformer[UpdateError]

object SkijaUpdateTransformer:
  given[IO[_] : Monad, UpdateError, State] : BiMonad[[A, B] =>> SkijaUpdateTransformer[UpdateError, State, List[A]][IO, B]] =
    [T] => () => summon

  def liftK[IO[_] : Monad, UpdateError, State, Events : Monoid]: IO ~> SkijaUpdateTransformer[UpdateError, State, Events][IO, *] =
    MonadTransformer[SkijaUpdateTransformer[UpdateError, State, Events]].liftK
  end liftK

  def getState[IO[_] : Monad, UpdateError, State, Events: Monoid]: SkijaUpdateTransformer[UpdateError, State, Events][IO, State] =
    StateTransformer.get_
  end getState

  def setState[IO[_] : Monad, UpdateError, State, Events: Monoid](state: State): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.set_(state)
  end setState

  def updateState[IO[_] : Monad, UpdateError, State, Events: Monoid](f: State => State): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.modify_(f)
  end updateState

  def raiseError[IO[_] : Monad, UpdateError, State, Events: Monoid, A](error: UpdateError): SkijaUpdateTransformer[UpdateError, State, Events][IO, A] =
    ErrorTransformer.raiseError(error)
  end raiseError

  def emitEvents[IO[_] : Monad, UpdateError, State, Events: Monoid](events: Events): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def catchEvents[IO[_] : Monad, UpdateError, State, Events : Monoid, NewEvents : Monoid]
      : [T] => SkijaUpdateTransformer[UpdateError, State, Events][IO, T] => SkijaUpdateTransformer[UpdateError, State, NewEvents][IO, (T, Events)] =
    [T] => update => EventsTransformer.catchEvents(update)
  end catchEvents

  def mapEvents[IO[_] : Monad, UpdateError, State, Events : Monoid, NewEvents : Monoid](f : Events => NewEvents)
      : SkijaUpdateTransformer[UpdateError, State, Events][IO, *] ~> SkijaUpdateTransformer[UpdateError, State, NewEvents][IO, *] =
    new ~>[
      SkijaUpdateTransformer[UpdateError, State, Events][IO, *], SkijaUpdateTransformer[UpdateError, State, NewEvents][IO, *]
    ]:
      override def apply[A](fa: SkijaUpdateTransformer[UpdateError, State, Events][IO, A]): SkijaUpdateTransformer[UpdateError, State, NewEvents][IO, A] =
        for
          tmp <- catchEvents[IO, UpdateError, State, Events, NewEvents][A](fa)
          (result, events) = tmp
          _ <- emitEvents(f(events))
        yield result
      end apply
    end new
  end mapEvents

  def getCornerCoordinates[IO[_] : Monad, UpdateError, Point, Clip, Events : Monoid] : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Point] =
    getState.map(_.widgetCoordinates)
  end getCornerCoordinates

  def getCornerCoordinates2d[IO[_] : Monad, UpdateError, MeasurementUnit, Clip, Events : Monoid] : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point3d[MeasurementUnit], Clip], Events][IO, Point2d[MeasurementUnit]] =
    getCornerCoordinates.map(_.projectToXY)
  end getCornerCoordinates2d

  def getClip[IO[_] : Monad, UpdateError, Point, Clip, Events : Monoid] : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Clip] =
    getState.map(_.path)
  end getClip

  def setClip[IO[_] : Monad, UpdateError, Point, Clip, Events : Monoid](clip : Clip) : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Unit] =
    updateState(_.withClip(clip))
  end setClip

  def markEventHandled[IO[_] : Monad, UpdateError, Point, Clip, Events : Monoid] : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Unit] =
    updateState(_.markEventHandled)
  end markEventHandled

  def isEventHandled[IO[_] : Monad, UpdateError, Point, Clip, Events : Monoid] : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Boolean] =
    getState.map(_.consumed)
  end isEventHandled

  def withCornerCoordinates[
    IO[_] : Monad,
    UpdateError,
    Point,
    Clip,
    Events : Monoid,
    Value
  ](
      original : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Value],
      f : Point => Point
  ) : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      _ <- updateState[IO, UpdateError, UpdateEffectState[Point, Clip], Events](_.withCoordinates(f(coordinates)))
      res <- original
      _ <- updateState[IO, UpdateError, UpdateEffectState[Point, Clip], Events](_.withCoordinates(coordinates))
    yield res
  end withCornerCoordinates

  def withClip[
    IO[_] : Monad,
    UpdateError,
    Point,
    Clip,
    Events : Monoid,
    Value
  ](
      original : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Value],
      f : (Clip, Point) => Clip
  ) : SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], Events][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      clip <- getClip
      _ <- setClip(f(clip, coordinates))
      res <- original
      _ <- setClip(clip)
    yield res
  end withClip

  def run[
    IO[_] : Monad,
    State,
    UpdateError,
    Events
  ](
    initialState : State,
  ) : [T] => SkijaUpdateTransformer[UpdateError, State, Events][IO, T] => IO[Either[UpdateError, (Events, (State,  T))]] =
    [T] => update =>
      update.run(initialState).run.value
  end run

  def handleApplicationRequests[
    IO[_] : Monad,
    Point : Monoid,
    Clip : Monoid,
    UpdateError,
  ](updateErrorAsExitCode : UpdateError => IO[ExitCode])
      : [T] => SkijaUpdateTransformer[UpdateError, UpdateEffectState[Point, Clip], List[SkijaApplicationRequest]][IO, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      run(UpdateEffectState.empty[Point, Clip])(update).flatMap {
        case Right((events, (_, widget))) =>
          events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
            request match
              case SkijaApplicationRequest.CloseApp(code) => Left(code).pure[IO]
          )
        case Left(error) =>
          updateErrorAsExitCode(error).map(Left(_))
      }
  end handleApplicationRequests
end SkijaUpdateTransformer
