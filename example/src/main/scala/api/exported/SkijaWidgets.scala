package me.katze.gui4s.example
package api.exported

import catnip.BiMonad
import cats.Monad
import cats.data.StateT
import cats.effect.ExitCode
import me.*
import me.katze.gui4s.example.EventResult
import me.katze.gui4s.example.api.SkijaWidget_
import me.katze.gui4s.example.update.ApplicationRequest
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.{Measurable, MeasurableT}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path}
import cats.syntax.all.*

opaque type Update[Event, Value] = EventResult[Event, Value]
given BiMonad[Update] = summon
given CatchEvents[Update] = summon


def runEventReaction[T, Event](reaction : EventReaction[T, Event, ?], path : Path) : Update[Event, T] =
  StateT(isEventHandled => EventResult_((isEventHandled, reaction.newState), reaction.parentEvent))
end runEventReaction

def handleApplicationRequests[F[_] : Monad] : [T] => UpdateT[ApplicationRequest][T] => F[Either[ExitCode, T]] =
  [T] => update =>
    val reaction : EventResult_[ApplicationRequest, (Boolean, T)] = update.run(false)
    reaction.events.foldM(Right(reaction.widget._2))((_, request) =>
      request match
        case ApplicationRequest.CloseApp(code) => Left(code).pure[F]
    )
end handleApplicationRequests

type UpdateT[UpEvent] = [Value] =>> Update[UpEvent, Value]
type Recomposition[F[_]] = F[Unit]
type PlacedWidget[F[+_], +Event, -DownEvent] =  SkijaWidget_[UpdateT[Event],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], +Event, -DownEvent] = Measurable[F, Float, PlacedWidget[F, Event, DownEvent]]