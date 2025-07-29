package me.katze.gui4s.example

import update.*

import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.{Async, Concurrent, ExitCode, Ref}
import cats.syntax.all.given
import cats.{Monad, MonadError}

import scala.concurrent.ExecutionContext

/**
 * Принимает способ получить нынешнее дерево виджетов и возвращает бесконечный цикл отрисовки. Завершается только в случае ошибки.
 */
type DrawLoop[F[+_], -Widget] = F[Widget] => F[ExitCode]

def runDrawLoopOnExecutionContext[F[+_]: Async, Widget](loop : DrawLoop[F, Widget], context : ExecutionContext) : DrawLoop[F, Widget] =
  widgetSource => loop(widgetSource).evalOn(context)
end runDrawLoopOnExecutionContext

/**
 * Принимает изначальный виджет, способ послать его обновлённую версию и способ получить следующее событие для обновления(может приостановить поток).
 */
type UpdateLoop[F[_], Widget, DownEvent] = (Widget, Widget => F[Unit], F[DownEvent]) => F[ExitCode]

def runUpdateLoopOnExecutionContext[F[_]: Async, Widget, HandleableEvent](loop : UpdateLoop[F, Widget, HandleableEvent], context : ExecutionContext) : UpdateLoop[F, Widget, HandleableEvent] =
  (widget, sink, eventSource) => loop(widget, sink, eventSource).evalOn(context)
end runUpdateLoopOnExecutionContext

/**
 * Каррированная версия MonadError.
 */
type MonadErrorT[T] = [F[_]] =>> MonadError[F, T]

/**
 * Запускает в отдельных потоках обновление виджета и его отрисовку.
 */
def applicationLoop[
  F[+_] : Concurrent, 
  DownEvent, 
  Widget
](
    eventBus     : Queue[F, DownEvent],
    widgetCell   : Ref[F, Widget],
    drawLoop     : DrawLoop[F, Widget],
    updateLoop   : UpdateLoop[F, Widget, DownEvent]
): F[ApplicationControl[F, DownEvent]] =
  for
    initialWidget <- widgetCell.get
    fork <- 
      Concurrent[F]
        .race(
          updateLoop(initialWidget, widgetCell.set, eventBus.take),
          drawLoop(widgetCell.get)
        )
        .map(_.fold(identity, identity))
        .start
  yield ApplicationControl(
    fork.cancel,
    fork.joinWithNever,
    eventBus.offer
  )
end applicationLoop

type DrawLoopExceptionHandler[F[_], Error] = Error => F[Option[ExitCode]]

def drawLoop[
  F[_] : MonadErrorT[Error],
  Error
](renderExceptionHandler : DrawLoopExceptionHandler[F, Error], shouldContinue : F[Boolean])(drawCurrentWidget : F[Unit]) : F[Option[ExitCode]] =
  runWhileNoError(
    drawCurrentWidget,
    renderExceptionHandler,
    shouldContinue
  )
end drawLoop


def runWhileNoError[F[_] : MonadErrorT[InternalError], InternalError, ExternalError](
                                                                                      effect: F[Unit],
                                                                                      recover: InternalError => F[Option[ExternalError]], 
                                                                                      shouldContinue: F[Boolean]
                                                                                    ): F[Option[ExternalError]] =
  Monad[F].tailRecM[None.type, Option[ExternalError]](
    None
  )(_ =>
    shouldContinue.flatMap(continue =>
      if continue then
        effect
          .as(Left(None))
          .handleErrorWith(error => recover(error).map(_.toRight(None).map(Some(_))))
      else
        Right(None).pure[F]
    )
  )
end runWhileNoError

def updateLoop[
                F[_] : Monad,
                Update[_],
                PlacedWidget <: EventConsumer[Update, F[PlacedWidget], DownEvent],
                DownEvent
              ](
                runUpdate  : [A] => Update[A] => F[Either[ExitCode, A]]
              )(
                initial: PlacedWidget,
                pushNew: PlacedWidget => F[Unit],
                nextEvent: F[DownEvent],
              ) : F[ExitCode] =
  Monad[F].tailRecM(initial)(
    updateStep(_, nextEvent, runUpdate) >>= doIfLeft(pushNew)
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
              Update[_],
              PlacedWidget <: EventConsumer[Update, F[PlacedWidget], DownEvent],
              DownEvent
            ](
                widget     : PlacedWidget,
                eventSource: F[DownEvent],
                runUpdate  : [A] => Update[A] => F[Either[ExitCode, A]]
            ): F[Either[PlacedWidget, ExitCode]] =
  for
    event         <- eventSource
    exitCodeOrWidget   <- runUpdate[F[PlacedWidget]](widget.processEvent(event))
    res <- exitCodeOrWidget match
      case Left(value) => Right(value).pure[F]
      case Right(value) => value.map(Left(_))
  yield res
end updateStep
