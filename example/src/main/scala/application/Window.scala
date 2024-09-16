package me.katze.gui4s.example
package application

import application.{*, given}

import cats.*
import cats.effect.kernel.{GenSpawn, RefSource}
import cats.effect.std.*
import cats.effect.syntax.all.{*, given}
import cats.effect.{IO as CatsIO, *}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.TaskFinished

import scala.concurrent.ExecutionContext

/**
 * Запускает приложение в окне. Работает в вызывающем потоке.
 * @param mainWidget главный виджет
 * @param systemEventQueue очередь системных событий. Принимается извне, так как может быть полезно мочь добавлять в неё извне.
 * @return очередь системных событий и файбер, ссылающийся на выполнение программы.
 */
def Window[
  F[+_]
    : DrawLoopT[Draw]
    : Async,
  Draw,
  PWidget[+A, -B] <: PlacedWidget[Draw, WidgetTaskImpl[F, A], [C, D] =>> Placeable[PWidget[C, D]], A, B],
  UpEvent,
  DownEvent >: TaskFinished
](
    mainWidget      : PWidget[UpEvent, DownEvent],
    systemEventQueue: Queue[F, DownEvent],
    ioQueue         : IOQueue[F]
)(
  using UpdateThreadLoop[F, Draw, PWidget]
): F[ExitCode] =
  for
    currentWidget <- Ref.of(mainWidget)
    code <- Async[F].race[ExitCode, ExitCode](
      updateThreadLoop(currentWidget, systemEventQueue, ioQueue),
      drawLoop[F, Draw](currentWidget)
    )
  yield code.fold[ExitCode](identity, identity)
end Window

