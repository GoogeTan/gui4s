package me.katze.gui4s.example
package application

import cats.Applicative
import cats.effect.ExitCode

trait ProcessUpEvent[F[+_], UpEvent]:
  def processUpEvent(ioQueue: IOQueue[F], event: UpEvent) : F[Option[ExitCode]]
end ProcessUpEvent


def processUpEvent[F[+_], UpEvent](ioQueue: IOQueue[F], event: UpEvent)(using p: ProcessUpEvent[F, UpEvent]): F[Option[ExitCode]] =
  p.processUpEvent(ioQueue, event)
end processUpEvent
