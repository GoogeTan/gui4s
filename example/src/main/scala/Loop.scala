package me.katze.gui4s.example

import draw.{Drawable, SimpleDrawApi}
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
type UpdateLoop[F[+_], Widget[_], DownEvent] = (Widget[DownEvent], Widget[DownEvent] => F[Unit], F[DownEvent]) => F[ExitCode]

/**
 * Каррированная версия MonadError.
 */
type MonadErrorT[T] = [F[_]] =>> MonadError[F, T]

/**
 * Запускает в отдельных потоках обновление виджета и его отрисовку.
 * @param freeRoot дерево виджетов
 * @param drawLoop Цикл отрисовки приложения. Например, может рисовать на экран, рендерить в html и тому подобное.
 * @param updateLoop цикл обновления дерева виджетов приложения.
 * @tparam DownEvent Тип событий, которые умеет обрабатывать виджет.
 */
def applicationLoop[
  F[+_] : Concurrent, 
  DownEvent, 
  WidgetPlaced[_],
  WidgetFree[T] <: Placeable[F, WidgetPlaced[T]]
](
    freeRoot: WidgetFree[DownEvent],
    drawLoop: DrawLoop[F, WidgetPlaced[DownEvent]],
    updateLoop: UpdateLoop[F, WidgetPlaced, DownEvent]
): F[ApplicationControl[F, DownEvent]] =

  for
    bus <- Queue.unbounded[F, DownEvent]
    root <- freeRoot.place()
    widget <- AtomicCell[F].of(root)
    fork <- 
      Concurrent[F]
        .race(
          updateLoop(root, widget.set, bus.take),
          drawLoop(widget.get)
        )
        .map(_.fold(identity, identity))
        .start
  yield ApplicationControl(
    fork.cancel,
    fork.joinWithNever,
    bus.offer
  )
end applicationLoop

type DrawLoopExceptionHandler[F[_], Error] = Error => F[Option[ExitCode]]

def drawLoop[
  F[+_] : MonadErrorT[Error],
  Error
](renderExceptionHandler : DrawLoopExceptionHandler[F, Error], api : SimpleDrawApi[F])(currentWidget : F[Drawable[F[Unit]]]) : F[ExitCode] =
  Monad[F].iterateWhile(
      (api.beginDraw *> currentWidget.flatMap(_.draw) *> api.endDraw)
        .as(None)
        .handleErrorWith(renderExceptionHandler)
    )(_.isEmpty)
    // Мы всегда уверены, что там Some, так как это условие выхода из цикла
    .map(_.get)
end drawLoop

trait Placeable[+F[+_], +T]:
  def place() : F[T]

def updateLoop[
                F[+_] : ProcessRequest : Monad,
                PlacedWidget[A] <: EventConsumer[FreeWidget[A], F, A, ApplicationRequest],
                FreeWidget[A] <: Placeable[F, PlacedWidget[A]],
                DownEvent
              ](
                initial: PlacedWidget[DownEvent],
                pushNew: PlacedWidget[DownEvent] => F[Unit],
                nextEvent: F[DownEvent],
              ) : F[ExitCode] =
  Monad[F].tailRecM(initial)(updateStep(_, nextEvent, pushNew))
end updateLoop

/**
 * TODO Написать норм описание, что тут происходит. А лучше поработать над неймингом, чтобы вопросов не возникало
 * @param widget Виджет, который принимает внешние события
 * @param waitForTheNextEvent Достаёт следующее событие из очереди или иного источника
 * @param pushNew Отправляет обновлённый виджет
 * @tparam DownEvent Тип внешнего события виджета
 * @return
 */
def updateStep[
              F[+_] : Monad : ProcessRequest,
              PlacedWidget[A] <: EventConsumer[FreeWidget[A], F, A, ApplicationRequest],
              FreeWidget[A] <: Placeable[F, PlacedWidget[A]],
              DownEvent
            ](
                widget: PlacedWidget[DownEvent],
                waitForTheNextEvent: F[DownEvent],
                pushNew: PlacedWidget[DownEvent] => F[Unit]
            ): F[Either[PlacedWidget[DownEvent], ExitCode]] =
  for
    event  <- waitForTheNextEvent
    processResult <- widget.processEvent(event)
    EventProcessResult(freeWidget, events) = processResult
    placedWidget <- freeWidget.place()
    _      <- pushNew(placedWidget)
    exit   <- processRequests(events)
  yield exit.toRight(placedWidget)
end updateStep

def processRequests[F[_] : Monad : ProcessRequest](requests : List[ApplicationRequest]) : F[Option[ExitCode]] =
  requests.collectFirstSomeM(_.process)
end processRequests
