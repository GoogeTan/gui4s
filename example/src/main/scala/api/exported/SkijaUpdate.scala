package me.katze.gui4s.example
package api.exported

import update.ApplicationRequest

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import catnip.syntax.bi.{stateWrapsBiMonad, writerIsBiMonad}
import cats.arrow.FunctionK
import cats.data.{EitherT, StateT, WriterT}
import cats.effect.ExitCode
import cats.syntax.all.*
import cats.{Applicative, Monad}
import io.github.humbleui.skija.{PathFillMode, PathOp, Path as SkijaPath}
import me.katze.gui4s.geometry.{Point2d, Point3d}
import me.katze.gui4s.widget.{CatchEvents, given}

final case class UpdateEffectState[MeasurementUnit](consumed : Boolean, widgetCoordinates : Point3d[MeasurementUnit], path: SkijaPath):
  def withCoordinates(point: Point3d[MeasurementUnit]): UpdateEffectState[MeasurementUnit] =
    copy(widgetCoordinates = point)
  end withCoordinates

  def markEventHandled : UpdateEffectState[MeasurementUnit] =
    copy(consumed = true)
  end markEventHandled

  def cliped(path : SkijaPath) : UpdateEffectState[MeasurementUnit] =
    copy(path = SkijaPath.makeCombining(this.path, path, PathOp.INTERSECT))
  end cliped
end UpdateEffectState

object UpdateEffectState:
  def empty[MeasurementUnit : Numeric as N] : UpdateEffectState[MeasurementUnit] =
    UpdateEffectState(false, Point3d(N.zero, N.zero, N.zero), new SkijaPath().setFillMode(PathFillMode.INVERSE_WINDING))
  end empty
end UpdateEffectState

opaque type SkijaUpdate[IO[_], MeasurementUnit, UpdateError, Event, Value] = EitherT[StateT[WriterT[IO, List[Event], *], UpdateEffectState[MeasurementUnit], *], UpdateError, Value]
type SkijaUpdateT[IO[_], MeasurementUnit, UpdateError, Event] = SkijaUpdate[IO, MeasurementUnit, UpdateError, Event, *]

object SkijaUpdate:
      given[IO[_] : Monad, MeasurementUnit, Clip, UpdateError] : CatchEvents[SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, *, *]] =
    liftEitherTCatchEvents[
      [A, B] =>> StateT[WriterT[IO, List[A], *], UpdateEffectState[MeasurementUnit, Clip], B],
      UpdateError
    ](using
      stateWrapsBiMonad[[Event, Value] =>> WriterT[IO, List[Event], Value], UpdateEffectState[MeasurementUnit, Clip]](using writerIsBiMonad),
      liftStateTCatchEvents[[A, B] =>> WriterT[IO, List[A], B], UpdateEffectState[MeasurementUnit, Clip]](using writerIsBiMonad)
    )

  def liftF[IO[_] : Monad, MeasurementUnit, Clip, UpdateError, Event, Value](io : IO[Value]) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Value] =
    liftK(io)
  end liftF

  def liftK[IO[_] : Monad, MeasrementUnit, Clip, UpdateError, Event] : FunctionK[IO, SkijaUpdateT[IO, MeasrementUnit, Clip, UpdateError, Event]] =
    WriterT.liftK[IO, List[Event]].andThen(StateT.liftK[WriterT[IO, List[Event], *], UpdateEffectState[MeasrementUnit, Clip]].andThen(EitherT.liftK))
  end liftK

      given skijaUpdateBiMonad[IO[_] : Monad, MeasurementUnit, Clip, UpdateError] : BiMonad[SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, *, *]] =
    eitherWrapsBiMonad[
      [A, B] =>> StateT[WriterT[IO, List[A], *], UpdateEffectState[MeasurementUnit, Clip], B],
      UpdateError
    ](
      using stateWrapsBiMonad[[A, B] =>> WriterT[IO, List[A], B], UpdateEffectState[MeasurementUnit, Clip]](using writerIsBiMonad)
    )

  def markEventHandled[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event] : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Unit] =
    EitherT.liftF(StateT.modify(_.markEventHandled))
  end markEventHandled

  def isEventHandled[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event] : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Boolean] =
    EitherT.liftF(StateT.get[WriterT[IO, List[Event], *], UpdateEffectState[MeasurementUnit, Clip]].map(_.consumed))
  end isEventHandled

  def getCoordinates[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event] : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Point3d[MeasurementUnit]] =
    EitherT.liftF(StateT.get[WriterT[IO, List[Event], *], UpdateEffectState[MeasurementUnit, Clip]].map(_.widgetCoordinates))
  end getCoordinates

  def getCoordinates2d[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event] : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Point2d[MeasurementUnit]] =
    getCoordinates.map(_.projectToXY)
  end getCoordinates2d

  def setCoordinates[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event](coordinates : Point3d[MeasurementUnit]) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Unit] =
    EitherT.liftF(
      StateT.modify(_.setCoordinates(coordinates))
    )
  end setCoordinates

  /*
  def clip[IO[_] : Applicative, MeasurementUnit, UpdateError, Event](path : SkijaPath) : SkijaUpdate[IO, MeasurementUnit, UpdateError, Event, Unit] =
    getCoordinates2d.flatMap:
      point2d =>
        EitherT.liftF(
          StateT.modify(_.clip(path.moveTo(point2d.x, point2d.y)))
        )
  end clip*/

  def withCoordinates[
    IO[_] : Monad,
    MeasurementUnit : Numeric,
    Clip,
    UpdateError,
    Event,
    Value
  ](
    update : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Value]
  )(
    transformation : Point3d[MeasurementUnit] => Point3d[MeasurementUnit]
  ) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Value] =
    for
      initial <- getCoordinates
      _ <- setCoordinates(transformation(initial))
      res <- update
      _ <- setCoordinates(initial)
    yield res
  end withCoordinates

  def raiseEvents[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event](events : List[Event]) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Unit] =
    EitherT.liftF(
      StateT.liftF(WriterT.tell(events))
    )
  end raiseEvents

  def raiseError[IO[_] : Applicative, MeasurementUnit, Clip, UpdateError, Event, Value](error : UpdateError) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event, Value] =
    EitherT.left(StateT.liftF(WriterT.liftF(error.pure[IO])))
  end raiseError

  def mapEvents[IO[_] : Monad, MeasurementUnit, Clip, UpdateError, Event1, Event2, T](f : Event1 => Event2)(skijaUpdate : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event1, T]) : SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, Event2, T] =
    skijaUpdate.catchEvents.flatMap((newEvents, value) => raiseEvents(newEvents.map(f)).as(value))
  end mapEvents

  def handleApplicationRequests[
    IO[_] : Monad,
    MeasurementUnit : Numeric as N,
    Clip,
    UpdateError,
  ](updateErrorAsExitCode : UpdateError => IO[ExitCode]) : [T] => SkijaUpdate[IO, MeasurementUnit, Clip, UpdateError, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      update.value.run(UpdateEffectState.empty).run.flatMap(result =>
        val (events, (_, maybeWidget)) = result
        maybeWidget match
          case Left(error) =>
            updateErrorAsExitCode(error).map(Left(_))
          case Right(widget) =>
            events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
              request match
                case ApplicationRequest.CloseApp(code) => Left(code).pure[IO]
            )
      )
  end handleApplicationRequests
end SkijaUpdate