package me.katze.gui4s.example
package api.effects

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import catnip.syntax.bi.{stateWrapsBiMonad, writerIsBiMonad}
import cats.arrow.FunctionK
import cats.data.{EitherT, StateT, WriterT}
import cats.effect.ExitCode
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.all.*
import cats.{Applicative, Monad, ~>}
import io.github.humbleui.skija.{PathFillMode, PathOp, Path as SkijaPath}
import me.katze.gui4s.example.api.effects.SkijaUpdate.catchEvents
import me.katze.gui4s.geometry.{Point2d, Point3d}
import me.katze.gui4s.widget.CatchEvents.catchEventsWriterT
import me.katze.gui4s.widget.{CatchEvents, given}
import catnip.transformer.*
import me.katze.gui4s.widget.library.effect.EventsTransformer

final case class UpdateEffectState[Point, Clip](consumed : Boolean, widgetCoordinates : Point, path: Clip):
  def withCoordinates(point: Point): UpdateEffectState[Point, Clip] =
    copy(widgetCoordinates = point)
  end withCoordinates

  def markEventHandled : UpdateEffectState[Point, Clip] =
    copy(consumed = true)
  end markEventHandled

  def withClip(path : Clip)(using M : Semigroup[Clip]) : UpdateEffectState[Point, Clip] =
    copy(path = M.combine(this.path, path))
  end withClip
end UpdateEffectState

object UpdateEffectState:
  def empty[Point : Monoid as N, Clip : Monoid as ClipM] : UpdateEffectState[Point, Clip] =
    UpdateEffectState(false, N.empty, ClipM.empty)
  end empty
end UpdateEffectState


opaque type SkijaUpdate[IO[_], S, UpdateError, Event, Value] =
  EitherT[StateT[WriterT[IO, List[Event], *], S, *], UpdateError, Value]

type SkijaUpdateT[IO[_], S, UpdateError, Event] = SkijaUpdate[IO, S, UpdateError, Event, *]

object SkijaUpdate:
  def catchEvents[IO[_] : Monad, S, UpdateError, Event1, Event2]: [T] => SkijaUpdate[IO, S, UpdateError, Event1, T] => SkijaUpdate[IO, S, UpdateError, Event2, (List[Event1], T)] =
    [A] => (update : SkijaUpdate[IO, S, UpdateError, Event1, A]) =>
      ???
  end catchEvents

  def run[IO[_] : Monad, S, UpdateError, Event, Value](value: SkijaUpdate[IO, S, UpdateError, Event, Value], initialState: S): IO[(List[Event], Either[UpdateError, Value])] =
    value.value.runA(initialState).run
  end run

  def liftF[IO[_] : Monad, S, UpdateError, Event, Value](io: IO[Value]): SkijaUpdate[IO, S, UpdateError, Event, Value] =
    liftK(io)
  end liftF

  def liftK[IO[_] : Monad, S, UpdateError, Event]: IO ~> SkijaUpdateT[IO, S, UpdateError, Event] =
    WriterT.liftK[IO, List[Event]].andThen(StateT.liftK[WriterT[IO, List[Event], *], S].andThen(EitherT.liftK))
  end liftK

  given skijaUpdateBiMonad[IO[_] : Monad, S, UpdateError]: BiMonad[SkijaUpdate[IO, S, UpdateError, *, *]] =
    eitherWrapsBiMonad[
      [A, B] =>> StateT[WriterT[IO, List[A], *], S, B],
      UpdateError
    ](
      using stateWrapsBiMonad[[A, B] =>> WriterT[IO, List[A], B], S](using writerIsBiMonad)
    )

  def markEventHandled[IO[_] : Applicative, Point, Clip, UpdateError, Event] : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Unit] =
    EitherT.liftF(StateT.modify(_.markEventHandled))
  end markEventHandled

  def isEventHandled[IO[_] : Applicative, Point, Clip, UpdateError, Event] : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Boolean] =
    EitherT.liftF(StateT.get[WriterT[IO, List[Event], *], UpdateEffectState[Point, Clip]].map(_.consumed))
  end isEventHandled

  def getState[IO[_] : Applicative, Point, Clip, UpdateError, Event] : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, UpdateEffectState[Point, Clip]] =
    EitherT.liftF(StateT.get[WriterT[IO, List[Event], *], UpdateEffectState[Point, Clip]])
  end getState

  def modifyState[
    IO[_] : Applicative,
    Point,
    Clip,
    UpdateError,
    Event
  ](
    f : UpdateEffectState[Point, Clip] => UpdateEffectState[Point, Clip]
  ) : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Unit] =
    EitherT.liftF(
      StateT.modify(f)
    )
  end modifyState

  def getCoordinates[IO[_] : Applicative, Point, Clip, UpdateError, Event] : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Point] =
    getState.map(_.widgetCoordinates)
  end getCoordinates

  def getCoordinates2d[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event] : SkijaUpdate[IO, UpdateEffectState[Point3d[MeasurementUnit], Clip], UpdateError, Event, Point2d[MeasurementUnit]] =
    getCoordinates.map(_.projectToXY)
  end getCoordinates2d

  def setCoordinates[IO[_] : Applicative, Point, Clip, UpdateError, Event](coordinates : Point) : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Unit] =
    modifyState(_.withCoordinates(coordinates))
  end setCoordinates

  def withClip[
    IO[_] : Monad,
    Point,
    Clip : Monoid,
    UpdateError,
    Event,
    Value
  ](
      path : Clip,
      original : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Value],
      clipAt : (Clip, Point) => Clip,
  ) : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Value] =
    getCoordinates.flatMap:
      point =>
        for
          clip <- getState[IO, Point, Clip, UpdateError, Event].map(_.path)
          _ <- modifyState[IO, Point, Clip, UpdateError, Event](_.withClip(clipAt(path, point)))
          result <- original
          _ <- modifyState[IO, Point, Clip, UpdateError, Event](_.withClip(clip))
        yield result
  end withClip

  def withCoordinates[
    IO[_] : Monad,
    Point,
    Clip,
    UpdateError,
    Event,
    Value
  ](
    update : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Value]
  )(
    transformation : Point => Point
  ) : SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, Event, Value] =
    for
      initial <- getCoordinates
      _ <- setCoordinates(transformation(initial))
      res <- update
      _ <- setCoordinates(initial)
    yield res
  end withCoordinates

  def raiseEvents[IO[_] : Applicative, S, UpdateError, Event](events: List[Event]): SkijaUpdate[IO, S, UpdateError, Event, Unit] =
    EitherT.liftF(
      StateT.liftF(WriterT.tell(events))
    )
  end raiseEvents

  def raiseError[IO[_] : Applicative, S, UpdateError, Event, Value](error: UpdateError): SkijaUpdate[IO, S, UpdateError, Event, Value] =
    EitherT.left(StateT.liftF(WriterT.liftF(error.pure[IO])))
  end raiseError

  def mapEvents[IO[_] : Monad, S, UpdateError, Event1, Event2, T](f: Event1 => Event2)(skijaUpdate: SkijaUpdate[IO, S, UpdateError, Event1, T]): SkijaUpdate[IO, S, UpdateError, Event2, T] =
    catchEvents(
      skijaUpdate
    ).flatMap((newEvents, value) => raiseEvents(newEvents.map(f)).as(value))
  end mapEvents

  def handleApplicationRequests[
    IO[_] : Monad,
    Point : Monoid,
    Clip : Monoid,
    UpdateError,
  ](updateErrorAsExitCode : UpdateError => IO[ExitCode]) : [T] => SkijaUpdate[IO, UpdateEffectState[Point, Clip], UpdateError, SkijaApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      update.value.run(UpdateEffectState.empty).run.flatMap(result =>
        val (events, (_, maybeWidget)) = result
        maybeWidget match
          case Left(error) =>
            updateErrorAsExitCode(error).map(Left(_))
          case Right(widget) =>
            events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
              request match
                case SkijaApplicationRequest.CloseApp(code) => Left(code).pure[IO]
            )
      )
  end handleApplicationRequests
end SkijaUpdate
