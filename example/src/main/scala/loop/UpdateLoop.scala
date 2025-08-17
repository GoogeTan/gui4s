package me.katze.gui4s.example
package loop

import cats.*
import cats.effect.*
import cats.syntax.all.{*, given}
import cats.effect.syntax.all.*

import scala.concurrent.ExecutionContext

/**
 * Принимает изначальный виджет, способ послать его обновлённую версию и способ получить следующее событие для обновления(может приостановить поток).
 */
type UpdateLoop[F[_], Widget, DownEvent] = (Widget, Widget => F[Unit], F[DownEvent]) => F[ExitCode]

def runUpdateLoopOnExecutionContext[F[_] : Async, Widget, HandleableEvent](loop: UpdateLoop[F, Widget, HandleableEvent], context: ExecutionContext): UpdateLoop[F, Widget, HandleableEvent] =
  (widget, sink, eventSource) => loop(widget, sink, eventSource).evalOn(context)
end runUpdateLoopOnExecutionContext

def updateLoop[
  F[_] : Monad,
  PlacedWidget,
  DownEvent
](
  processEvent : (PlacedWidget, DownEvent) => F[Either[ExitCode, F[PlacedWidget]]]
)(
  initial: PlacedWidget,
  pushNew: PlacedWidget => F[Unit],
  nextEvent: F[DownEvent],
) : F[ExitCode] =
  Monad[F].tailRecM(initial)(
    updateStep(_, nextEvent, processEvent) >>= doIfLeft(pushNew)
  )
end updateLoop

def doIfLeft[F[_] : Monad, A, B](f : A => F[Unit])(value : Either[A, B]) : F[Either[A, B]] =
  value match
    case Left(value)  => f(value).as(Left(value))
    case Right(value) => Right(value).pure[F]
  end match
end doIfLeft


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
  DownEvent
](
  widget     : PlacedWidget,
  eventSource: F[DownEvent],
  processEvent : (PlacedWidget, DownEvent) => F[Either[ExitCode, F[PlacedWidget]]]
): F[Either[PlacedWidget, ExitCode]] =
  eventSource.flatMap(event =>
    processEvent(widget, event)
  ).flatMap {
    case Left(code) => Right(code).pure[F]
    case Right(widget) => widget.map(Left(_))
  }
end updateStep
