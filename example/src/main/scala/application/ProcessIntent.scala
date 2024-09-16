package me.katze.gui4s.example
package application

import cats.effect.{ExitCode, IO}
import me.katze.gui4s.widget.impl.WidgetTaskImpl

trait ProcessIntent[F[+_], UpEvent]:
  def processIntent(ioQueue: IOQueue[F], intent: WidgetTaskImpl[F, UpEvent]) : F[Option[ExitCode]]
end ProcessIntent


def processIntent[F[+_], UpEvent](ioQueue: IOQueue[F], intent: WidgetTaskImpl[F, UpEvent])(using p: ProcessIntent[F, UpEvent]): F[Option[ExitCode]] =
  p.processIntent(ioQueue, intent)
end processIntent


