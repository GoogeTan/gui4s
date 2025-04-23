package me.katze.gui4s.example

import impl.given

import cats.*
import cats.effect.*
import cats.effect.std.{Queue, QueueSink}
import cats.syntax.all.*

def runApplicationLoopsWithBackend[
  F[+_] : Async,
  DownEvent,
  RootWidget[_],
  Backend
](
    backend : QueueSink[F, DownEvent] => Resource[F, Backend],
    drawLoop : Backend => DrawLoop[F, RootWidget[DownEvent]],
    updateLoop : Backend => UpdateLoop[F, RootWidget, DownEvent],
    rootWidget : Backend => F[RootWidget[DownEvent]]
) : F[ExitCode] =
  runApplicationLoops[
    F,
    DownEvent,
    RootWidget
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
  F[+_] : Async,
  DownEvent,
  RootWidget[_],
](
  loops: QueueSink[F, DownEvent] => Resource[F, (
      DrawLoop[F, RootWidget[DownEvent]],
      UpdateLoop[F, RootWidget, DownEvent],
      F[RootWidget[DownEvent]],
    )
  ],
) : F[ExitCode] =
  for
    eventBus <- Queue.unbounded[F, DownEvent]
    code <- loops(eventBus).use((drawLoop, updateLoop, freeRootWidget) =>
      for
        rootWidget <- freeRootWidget
        widgetCell <- Ref[F].of(rootWidget)
        code <- applicationLoop(
          eventBus,
          widgetCell,
          drawLoop,
          updateLoop,
        ).flatMap(_.join)
      yield code
    )
  yield code
end runApplicationLoops
