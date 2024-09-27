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
type UpdateLoop[F[+_], Widget[_, _], UpEvent, DownEvent] = (Widget[UpEvent, DownEvent], Widget[UpEvent, DownEvent] => F[Unit], F[DownEvent]) => F[ExitCode]

/**
 * Каррированная версия MonadError.
 */
type MonadErrorT[T] = [F[_]] =>> MonadError[F, T]

trait RootPlaceable[+F[+_], +T]:
  def place() : F[T]

/**
 * Запускает в отдельных потоках обновление виджета и его отрисовку.
 * @param initialWidget дерево виджетов
 * @param drawLoop Цикл отрисовки приложения. Например, может рисовать на экран, рендерить в html и тому подобное.
 * @param updateLoop цикл обновления дерева виджетов приложения.
 * @tparam DownEvent Тип событий, которые умеет обрабатывать виджет.
 */
def applicationLoop[
  F[+_] : Concurrent, 
  UpEvent,
  DownEvent, 
  RootWidgetPlaced[_, _],
  RootWidgetFree[A, B] <: RootPlaceable[F, RootWidgetPlaced[A, B]]
](
    initialWidget : Queue[F, DownEvent] => RootWidgetFree[UpEvent, DownEvent],
    drawLoop      : DrawLoop[F, RootWidgetPlaced[UpEvent, DownEvent]],
    updateLoop    : UpdateLoop[F, RootWidgetPlaced, UpEvent, DownEvent]
): F[ApplicationControl[F, DownEvent]] =

  for
    bus <- Queue.unbounded[F, DownEvent]
    root <- initialWidget(bus).place()
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

def updateLoop[
                F[+_] : Monad,
                PlacedRootWidget <: EventConsumer[FreeRootWidget, F, UpEvent, DownEvent],
                FreeRootWidget <: RootPlaceable[F, PlacedRootWidget],
                UpEvent,
                DownEvent
              ](
                  initial: PlacedRootWidget,
                  pushNew: PlacedRootWidget => F[Unit],
                  nextEvent: F[DownEvent],
              )(using ProcessRequest[F, UpEvent]) : F[ExitCode] =
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
              F[+_] : Monad,
              PlacedWidget <: EventConsumer[FreeWidget, F, UpEvent, DownEvent],
              FreeWidget <: RootPlaceable[F, PlacedWidget],
              UpEvent,
              DownEvent
            ](
                widget: PlacedWidget,
                waitForTheNextEvent: F[DownEvent],
                pushNew: PlacedWidget => F[Unit]
            )(using ProcessRequest[F, UpEvent]): F[Either[PlacedWidget, ExitCode]] =
  for
    event  <- waitForTheNextEvent
    processResult <- widget.processEvent(event)
    EventProcessResult(freeWidget, events) = processResult
    placedWidget <- freeWidget.place()
    _      <- pushNew(placedWidget)
    exit   <- processRequests(events)
  yield exit.toRight(placedWidget)
end updateStep

def processRequests[F[_] : Monad, UpEvent](requests : List[UpEvent])(using ProcessRequest[F, UpEvent]) : F[Option[ExitCode]] =
  requests.collectFirstSomeM(_.process)
end processRequests
