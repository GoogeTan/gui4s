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
import cats.data.{StateT, WriterT}
import cats.syntax.all.*
import me.katze.gui4s.widget.{CatchEvents, given}

// TODO Переименовать в состояние обновления
final case class EventResultState[MeasurementUnit](consumed : Boolean, widgetCoordinates : Point3d[MeasurementUnit]):
  def addCoordinates(using Numeric[MeasurementUnit])(point : Point3d[MeasurementUnit]) : EventResultState[MeasurementUnit] =
    copy(widgetCoordinates = widgetCoordinates + point)
  end addCoordinates
end EventResultState

def emptyEventResultState[MeasurementUnit : Numeric as N] : EventResultState[MeasurementUnit] =
  EventResultState(false, Point3d(N.zero, N.zero, N.zero))
end emptyEventResultState

def markEventHandled[MeasuremementUnit](state : EventResultState[MeasuremementUnit]) : EventResultState[MeasuremementUnit] =
  EventResultState(true, state.widgetCoordinates)
end markEventHandled

opaque type SkijaUpdate[IO[_], MeasurementUnit, Event, Value] = StateT[WriterT[IO, List[Event], *], EventResultState[MeasurementUnit], Value]
type SkijaUpdateT[IO[_], MeasurementUnit, Event] = SkijaUpdate[IO, MeasurementUnit, Event, *]

given[IO[_] : Monad, MeasurementUnit] : CatchEvents[SkijaUpdate[IO, MeasurementUnit, *, *]] = liftStateTCatchEvents[[A, B] =>> WriterT[IO, List[A], B], EventResultState[MeasurementUnit]](using writerIsBiMonad)

def liftIOToSkijaUpdate[IO[_] : Monad, MeasurementUnit, Event, Value](io : IO[Value]) : SkijaUpdate[IO, MeasurementUnit, Event, Value] =
  StateT.liftF(WriterT.liftF(io))
end liftIOToSkijaUpdate

given[IO[_] : Monad, MeasurementUnit] : BiMonad[SkijaUpdate[IO, MeasurementUnit, *, *]] = stateWrapsBiMonad[[A, B] =>> WriterT[IO, List[A], B], EventResultState[MeasurementUnit]](using writerIsBiMonad)

def markEventHandled[IO[_] : Applicative, MeasurementUnit, Event] : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.modify(markEventHandled)
end markEventHandled

def getCoordinates[IO[_] : Applicative, MeasurementUnit, Event] : SkijaUpdate[IO, MeasurementUnit, Event, Point3d[MeasurementUnit]] =
  StateT.get[WriterT[IO, List[Event], *], EventResultState[MeasurementUnit]].map(_.widgetCoordinates)
end getCoordinates

def addCoordinates[IO[_] : Applicative, MeasurementUnit : Numeric, Event](coordinates : Point3d[MeasurementUnit]) : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.modify(_.addCoordinates(coordinates))
end addCoordinates

def raiseEvents[IO[_] : Applicative, MeasurementUnit, Event](events : List[Event]) : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.liftF(WriterT.tell(events))
end raiseEvents

def mapEvents[IO[_] : Monad, MeasurementUnit, Event1, Event2, T](f : Event1 => Event2)(skijaUpdate : SkijaUpdate[IO, MeasurementUnit, Event1, T]) : SkijaUpdate[IO, MeasurementUnit, Event2, T] =
  skijaUpdate.catchEvents.flatMap((newEvents, value) => raiseEvents(newEvents.map(f)).as(value))
end mapEvents

def runEventReaction[IO[_] : Monad, MeasurementUnit, T, Event](reaction : EventReaction[T, Event, ?], path : Path) : SkijaUpdate[IO, MeasurementUnit, Event, T] =
  StateT(isEventHandled => WriterT.tell(reaction.parentEvent).as((isEventHandled, reaction.newState)))
end runEventReaction

def handleApplicationRequests[IO[_] : Monad, MeasurementUnit : Numeric as N] : [T] => SkijaUpdate[IO, MeasurementUnit, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
  [T] => update =>
    update.run(emptyEventResultState).run.flatMap(result =>
      val (events, (_, widget)) = result
      events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
        request match
          case ApplicationRequest.CloseApp(code) => Left(code).pure[IO]
      )
    )
end handleApplicationRequests
