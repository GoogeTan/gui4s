package gui4s.core.loop

import scala.concurrent.ExecutionContext

import cats._
import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._

/**
 * Принимает изначальный виджет, способ послать его обновлённую версию и способ получить следующее событие для обновления(может приостановить поток).
 */
type UpdateLoop[F[_], Widget, DownEvent, ExitCode] = (Widget, Widget => F[Unit], F[DownEvent]) => F[ExitCode]

def runUpdateLoopOnExecutionContext[
  F[_] : Async,
  Widget,
  EnvironmentalEvent,
  ExitCode
](
   loop: UpdateLoop[F, Widget, EnvironmentalEvent, ExitCode],
   context: ExecutionContext
): UpdateLoop[F, Widget, EnvironmentalEvent, ExitCode] =
  (widget, sink, eventSource) => loop(widget, sink, eventSource).evalOn(context)
end runUpdateLoopOnExecutionContext

def updateLoop[
  F[_] : {Monad, Clock as C},
  PlacedWidget,
  DownEvent,
  ExitCode
](
  processEvent : (PlacedWidget, DownEvent) => F[Either[ExitCode, PlacedWidget]]
) : UpdateLoop[F, PlacedWidget, DownEvent, ExitCode] =
  (
  initial: PlacedWidget,
  pushNew: PlacedWidget => F[Unit],
  nextEvent: F[DownEvent],
  ) => Monad[F].tailRecM(initial)(currentWidget =>
    for
      time <- C.realTime
      res <- updateStep(currentWidget, nextEvent, processEvent).flatTap(doIfRight(pushNew)).map(_.swap)
      newTime <- C.realTime
      //TODO profile further _ = println((newTime - time).toMillis)
    yield res
  )
end updateLoop

def doIfRight[F[_] : Monad, A, B](f : B => F[Unit])(value : Either[A, B]) : F[Unit] =
  value match
    case Left(value)  => ().pure[F]
    case Right(value) => f(value)
  end match
end doIfRight

/**
 * Одна итерация обновления виджетов. Ожидает появления события, обновляет дерево и выполняет все запросы виджета.
 *
 * @param widget Виджет, который принимает внешние события
 * @param eventSource Даёт следующее событие. Возможно ожидание.
 * @return Left(widget), если обновление должно продолжиться, Right(ExitCode) иначе
 */
def updateStep[
  F[_] : Monad,
  PlacedWidget,
  DownEvent,
  ExitCode
](
  widget     : PlacedWidget,
  eventSource: F[DownEvent],
  processEvent : (PlacedWidget, DownEvent) => F[Either[ExitCode, PlacedWidget]]
): F[Either[ExitCode, PlacedWidget]] =
  eventSource.flatMap(event =>
    processEvent(widget, event)
  )
end updateStep
