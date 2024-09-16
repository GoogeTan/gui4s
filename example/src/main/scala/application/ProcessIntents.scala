package me.katze.gui4s.example
package application

import cats.effect.{Async, ExitCode, IO}
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.stateful.Path

trait ProcessIntents[F[+_], UpEvent]:
  def processIntents(ioQueue: IOQueue[F], intends: List[(Path, WidgetTaskImpl[F, UpEvent])]): F[Option[ExitCode]]
end ProcessIntents


def processIntents[
  F[+_],
  UpEvent
](
  ioQueue: IOQueue[F], 
  intends: List[(Path, WidgetTaskImpl[F, UpEvent])]
)(using p : ProcessIntents[F, UpEvent]): F[Option[ExitCode]] =
  p.processIntents(ioQueue, intends)
end processIntents
