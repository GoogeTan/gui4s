package me.katze.gui4s.example
package api.exported

import update.ApplicationRequest

import catnip.BiMonad
import cats.effect.ExitCode
import me.katze.gui4s.widget.{EventReaction, Path}
import cats.Applicative
import me.katze.gui4s.layout.Point3d
import catnip.syntax.all.{*, given}
import catnip.syntax.bi.{stateWrapsBiMonad, writerIsBiMonad}
import cats.Monad
import cats.data.{EitherT, StateT, WriterT}
import cats.effect.std.Supervisor
import cats.syntax.all.*
import me.katze.gui4s.widget.{CatchEvents, given}

final case class UpdateEffectState[MeasurementUnit](consumed : Boolean, widgetCoordinates : Point3d[MeasurementUnit]):
  def addCoordinates(using Numeric[MeasurementUnit])(point : Point3d[MeasurementUnit]) : UpdateEffectState[MeasurementUnit] =
    copy(widgetCoordinates = widgetCoordinates + point)
  end addCoordinates
end UpdateEffectState

def emptyUpdateState[MeasurementUnit : Numeric as N] : UpdateEffectState[MeasurementUnit] =
  UpdateEffectState(false, Point3d(N.zero, N.zero, N.zero))
end emptyUpdateState

def markEventHandled[MeasuremementUnit](state : UpdateEffectState[MeasuremementUnit]) : UpdateEffectState[MeasuremementUnit] =
  UpdateEffectState(true, state.widgetCoordinates)
end markEventHandled

opaque type SkijaUpdate[IO[_], UpdateError, MeasurementUnit, Event, Value] = EitherT[StateT[WriterT[IO, List[Event], *], UpdateEffectState[MeasurementUnit], *], UpdateError, Value]
type SkijaUpdateT[IO[_], UpdateError, MeasurementUnit, Event] = SkijaUpdate[IO, UpdateError, MeasurementUnit, Event, *]

given[IO[_] : Monad, UpdateError, MeasurementUnit] : CatchEvents[SkijaUpdate[IO, UpdateError, MeasurementUnit, *, *]] =
  liftEitherTCatchEvents[
    [A, B] =>> StateT[WriterT[IO, List[A], *], UpdateEffectState[MeasurementUnit], B],
    UpdateError
  ](using
    stateWrapsBiMonad[[Event, Value] =>> WriterT[IO, List[Event], Value], UpdateEffectState[MeasurementUnit]](using writerIsBiMonad),
    liftStateTCatchEvents[[A, B] =>> WriterT[IO, List[A], B], UpdateEffectState[MeasurementUnit]](using writerIsBiMonad)
  )

def liftIOToSkijaUpdate[IO[_] : Monad,  UpdateError, MeasurementUnit, Event, Value](io : IO[Value]) : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event, Value] =
  EitherT.liftF(StateT.liftF(WriterT.liftF(io)))
end liftIOToSkijaUpdate

given skijaUpdateBiMonad[IO[_] : Monad, UpdateError, MeasurementUnit] : BiMonad[SkijaUpdate[IO,  UpdateError, MeasurementUnit, *, *]] =
  eitherWrapsBiMonad[
    [A, B] =>> StateT[WriterT[IO, List[A], *], UpdateEffectState[MeasurementUnit], B],
    UpdateError
  ](
    using stateWrapsBiMonad[[A, B] =>> WriterT[IO, List[A], B], UpdateEffectState[MeasurementUnit]](using writerIsBiMonad)
  )

def markEventHandled[IO[_] : Applicative, UpdateError, MeasurementUnit, Event] : SkijaUpdate[IO, UpdateError, MeasurementUnit, Event, Unit] =
  EitherT.liftF(StateT.modify(markEventHandled))
end markEventHandled

def getCoordinates[IO[_] : Applicative,  UpdateError, MeasurementUnit, Event] : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event, Point3d[MeasurementUnit]] =
  EitherT.liftF(StateT.get[WriterT[IO, List[Event], *], UpdateEffectState[MeasurementUnit]].map(_.widgetCoordinates))
end getCoordinates

def addCoordinates[IO[_] : Applicative,  UpdateError, MeasurementUnit : Numeric, Event](coordinates : Point3d[MeasurementUnit]) : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event, Unit] =
  EitherT.liftF(
    StateT.modify(_.addCoordinates(coordinates))
  )
end addCoordinates

def raiseEvents[IO[_] : Applicative,  UpdateError, MeasurementUnit, Event](events : List[Event]) : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event, Unit] =
  EitherT.liftF(
    StateT.liftF(WriterT.tell(events))
  )
end raiseEvents

def raiseErrorInUpdate[IO[_] : Applicative, UpdateError, MeasurementUnit, Event, Value](error : UpdateError) : SkijaUpdate[IO, UpdateError, MeasurementUnit, Event, Value] =
  EitherT.left(StateT.liftF(WriterT.liftF(error.pure[IO])))
end raiseErrorInUpdate

def mapEvents[IO[_] : Monad,  UpdateError, MeasurementUnit, Event1, Event2, T](f : Event1 => Event2)(skijaUpdate : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event1, T]) : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event2, T] =
  skijaUpdate.catchEvents.flatMap((newEvents, value) => raiseEvents(newEvents.map(f)).as(value))
end mapEvents

def runEventReaction[IO[_] : Monad,  UpdateError, MeasurementUnit, T, Event](reaction : EventReaction[T, Event, Path => IO[Unit]], path : Path) : SkijaUpdate[IO,  UpdateError, MeasurementUnit, Event, T] =
  EitherT.liftF(
    StateT(isEventHandled =>
      WriterT.tell(reaction.parentEvent).as((isEventHandled, reaction.newState))
        <* WriterT.liftF(reaction.ios.traverse_(_(path)))
    )
  )
end runEventReaction

def handleApplicationRequests[IO[_] : Monad,  UpdateError, MeasurementUnit : Numeric as N](updateErrorAsExitCode : UpdateError => IO[ExitCode]) : [T] => SkijaUpdate[IO,  UpdateError, MeasurementUnit, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
  [T] => update =>
    update.value.run(emptyUpdateState).run.flatMap(result =>
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
