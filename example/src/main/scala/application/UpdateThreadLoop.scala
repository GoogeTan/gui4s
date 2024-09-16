package me.katze.gui4s.example
package application

import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.TaskFinished

trait UpdateThreadLoop[
  F[+_],
  Draw,
  PWidget[+A, -B] <: PlacedWidget[Draw, WidgetTaskImpl[F, A], [C, D] =>> Placeable[PWidget[C, D]], A, B],
]:
  def updateThreadLoop[UpEvent, DownEvent >: TaskFinished](
    window : Ref[F, PWidget[UpEvent, DownEvent]],
    eventQueue : QueueSource[F, DownEvent],
    ioQueue: IOQueue[F]
  ) : F[ExitCode]
end UpdateThreadLoop

def updateThreadLoop[
  F[+_],
  Draw,
  PWidget[+A, -B] <: PlacedWidget[Draw, WidgetTaskImpl[F, A], [C, D] =>> Placeable[PWidget[C, D]], A, B],
  UpEvent,
  DownEvent >: TaskFinished
](
    window    : Ref[F, PWidget[UpEvent, DownEvent]],
    eventQueue: QueueSource[F, DownEvent],
    ioQueue   : IOQueue[F]
)(using U: UpdateThreadLoop[F, Draw, PWidget]): F[ExitCode]  =
  U.updateThreadLoop(window, eventQueue, ioQueue)
end updateThreadLoop
