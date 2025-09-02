package gui4s.core.loop

import cats.*
import cats.effect.*
import cats.effect.std.{Queue, QueueSink}
import cats.syntax.all.*

def runApplicationLoopsWithBackend[
  F[_] : Async,
  DownEvent,
  RootWidget,
  Backend,
  ExitCode
](
    backend : QueueSink[F, DownEvent] => Resource[F, Backend],
    drawLoop : Backend => DrawLoop[F, RootWidget, ExitCode],
    updateLoop : Backend => UpdateLoop[F, RootWidget, DownEvent, ExitCode],
    rootWidget : Backend => F[RootWidget]
) : F[ExitCode] =
  runApplicationLoops[
    F,
    DownEvent,
    RootWidget,
    ExitCode
  ](
    sink => backend(sink).map(state =>
      (
        drawLoop(state),
        updateLoop(state),
        rootWidget(state)
      )
    )
  )
end runApplicationLoopsWithBackend

def runApplicationLoops[
  F[_] : Async,
  DownEvent,
  RootWidget,
  ExitCode
](
  loops: QueueSink[F, DownEvent] => Resource[F, (
      DrawLoop[F, RootWidget, ExitCode],
      UpdateLoop[F, RootWidget, DownEvent, ExitCode],
      F[RootWidget],
    )
  ],
) : F[ExitCode] =
  Queue.unbounded[F, DownEvent].flatMap(eventBus =>
    loops(eventBus).use((drawLoop, updateLoop, freeRootWidget) =>
      freeRootWidget.flatMap(Ref[F].of).flatMap(
        applicationLoop(
          eventBus.take,
          _,
          drawLoop,
          updateLoop,
        )
      )
    )
  )  
end runApplicationLoops

/**
 * Запускает в отдельных потоках обновление виджета и его отрисовку.
 */
def applicationLoop[
  F[_] : Concurrent,
  DownEvent,
  Widget,
  ExitCode
](
  eventSource  : F[DownEvent],
  widgetCell   : Ref[F, Widget],
  drawLoop     : DrawLoop[F, Widget, ExitCode],
  updateLoop   : UpdateLoop[F, Widget, DownEvent, ExitCode]
): F[ExitCode] =
  for
    initialWidget <- widgetCell.get
    code <-
      Concurrent[F]
        .race(
          updateLoop(initialWidget, widgetCell.set, eventSource),
          drawLoop(widgetCell.get)
        )
        .map(_.fold(identity, identity))
  yield code
end applicationLoop
