package me.katze.gui4s.example
package syntax

import application.*

import cats.*
import cats.effect.std.QueueSource
import cats.effect.{ExitCode, Ref}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.placeable.{Bounds, Placeable}

def updateThreadLoop[
  F[+_] : Monad,
  Draw,
  PWidget[+A, -B] <: PlacedWidget[Draw, WidgetTaskImpl[F, A], [C, D] =>> Placeable[PWidget[C, D]], A, B],
  UpEvent,
  DownEvent
](
    window    : Ref[F, PWidget[UpEvent, DownEvent]],
    bounds    : Bounds,
    eventQueue: QueueSource[F, DownEvent],
    ioQueue   : IOQueue[F]
)(
  using ProcessIntents[F, UpEvent]
): F[ExitCode] =
  for
    currentEvent <- eventQueue.take
    intents      <- window.modify(widget =>
      val er = widget.handleDownEvent(currentEvent)
      (er.widget.place(bounds), (er.upEvent, er.ios))
    )
    maybeCode <- processIntents(ioQueue, intents._2)
    code      <- maybeCode.fold(updateThreadLoop(window, bounds, eventQueue, ioQueue))(_.pure[F])
  yield code
end updateThreadLoop
