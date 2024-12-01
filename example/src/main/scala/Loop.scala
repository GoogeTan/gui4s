package me.katze.gui4s.example

import update.*

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.syntax.all.{*, given}
import cats.effect.{Concurrent, ExitCode}
import cats.syntax.all.{*, given}
import cats.{Monad, MonadError}

/**
 * Принимает способ получить нынешнее дерево виджетов и возвращает бесконечный цикл отрисовки. Завершается только в случае ошибки.
 */
type DrawLoop[F[+_], -Widget] = F[Widget] => F[ExitCode]


/**
 * Принимает изначальный виджет, способ послать его обновлённую версию и способ получить следующее событие для обновления(может приостановить поток).
 */
type UpdateLoop[F[+_], Widget[_, _], UpEvent, DownEvent] = (Widget[UpEvent, DownEvent], Widget[UpEvent, DownEvent] => F[Unit], F[DownEvent]) => F[ExitCode]

/**
 * Каррированная версия MonadError.
 */
type MonadErrorT[T] = [F[_]] =>> MonadError[F, T]

/**
 * Запускает в отдельных потоках обновление виджета и его отрисовку.
 */
def applicationLoop[
  F[+_] : Concurrent, 
  UpEvent,
  DownEvent, 
  Widget[_, _]
](
    eventBus     : Queue[F, DownEvent],
    widgetCell   : AtomicCell[F, Widget[UpEvent, DownEvent]],
    drawLoop     : DrawLoop[F, Widget[UpEvent, DownEvent]],
    updateLoop   : UpdateLoop[F, Widget, UpEvent, DownEvent]
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
  F[+_] : MonadErrorT[Error],
  Error
](renderExceptionHandler : DrawLoopExceptionHandler[F, Error], beginDraw : F[Unit], endDraw : F[Unit])(drawCurrentWidget : F[Unit]) : F[ExitCode] =
  Monad[F].tailRecM[None.type, ExitCode](
    None
  )(_ =>
      (beginDraw *> drawCurrentWidget *> endDraw)
        .as(None)
        .handleErrorWith(renderExceptionHandler)
        .map(_.toRight(None))
  )
end drawLoop

def updateLoop[
                F[+_] : Monad,
                PlacedWidget <: EventConsumer[F[PlacedWidget], F, UpEvent, DownEvent],
                UpEvent,
                DownEvent
              ](
                  initial: PlacedWidget,
                  pushNew: PlacedWidget => F[Unit],
                  nextEvent: F[DownEvent],
              )(using ProcessRequest[F, UpEvent]) : F[ExitCode] =
  Monad[F].tailRecM(initial)(
    updateStep(_, nextEvent) >>= doIfLeft(pushNew)
  )
end updateLoop

def doIfLeft[F[_] : Monad, A, B](f : A => F[Unit])(value : Either[A, B]) : F[Either[A, B]] =
  value match
    case Left(value)  => f(value).map(_ => Left(value))
    case Right(value) => Right(value).pure[F]
  end match
end doIfLeft


/**
 * TODO Написать норм описание, что тут происходит. А лучше поработать над неймингом, чтобы вопросов не возникало
 *
 * @param widget Виджет, который принимает внешние события
 * @param eventSource Даёт следующее событие. Возможно ожидание.
 * @tparam DownEvent Тип внешнего события виджета
 * @return
 */
def updateStep[
              F[+_] : Monad,
              PlacedWidget <: EventConsumer[F[PlacedWidget], F, UpEvent, DownEvent],
              UpEvent,
              DownEvent
            ](
                widget     : PlacedWidget,
                eventSource: F[DownEvent],
            )(using ProcessRequest[F, UpEvent]): F[Either[PlacedWidget, ExitCode]] =
  for
    event         <- eventSource
    eventResult   <- widget.processEvent(event)
    placedWidget  <- eventResult.widget
    exit          <- processRequests(eventResult.events)
  yield exit.toRight(placedWidget)
end updateStep

def processRequests[F[_] : Monad, UpEvent](requests : List[UpEvent])(using ProcessRequest[F, UpEvent]) : F[Option[ExitCode]] =
  requests.collectFirstSomeM(_.process)
end processRequests
