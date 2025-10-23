package gui4s.core.loop

import cats.*
import cats.effect.*
import cats.effect.std.{Queue, QueueSink}
import cats.syntax.all.*
import catnip.resource.UseC

def runApplicationLoopsWithBackend[
  IO[_] : Async,
  Resource[_] : {Functor, UseC[IO]},
  QueueIO[_] : Concurrent,
  DownEvent,
  RootWidget,
  Backend,
  ExitCode
](
   backend : QueueSink[QueueIO, DownEvent] => Resource[Backend],
   drawLoop : Backend => DrawLoop[IO, RootWidget, ExitCode],
   updateLoop : Backend => UpdateLoop[IO, RootWidget, DownEvent, ExitCode],
   rootWidget : Backend => IO[RootWidget],
   liftIO : QueueIO ~> IO,
) : IO[ExitCode] =
  runApplicationLoops[
    IO,
    Resource,
    QueueIO,
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
    ),
    liftIO,
  )
end runApplicationLoopsWithBackend

def runApplicationLoops[
  IO[_] : Async,
  Resource[_] : UseC[IO],
  QueueIO[_] : Concurrent,
  DownEvent,
  RootWidget,
  ExitCode
](
  loops: QueueSink[QueueIO, DownEvent] => Resource[(
      DrawLoop[IO, RootWidget, ExitCode],
      UpdateLoop[IO, RootWidget, DownEvent, ExitCode],
      IO[RootWidget],
    )
  ],
  liftIO : QueueIO ~> IO,
) : IO[ExitCode] =
  liftIO(Queue.unbounded[QueueIO, DownEvent]).flatMap(eventBus =>
    loops(eventBus).use((drawLoop, updateLoop, freeRootWidget) =>
      freeRootWidget.flatMap(Ref[IO].of).flatMap(
        applicationLoop(
          liftIO(eventBus.take),
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
  IO[_] : Concurrent,
  DownEvent,
  Widget,
  ExitCode
](
   eventSource  : IO[DownEvent],
   widgetCell   : Ref[IO, Widget],
   drawLoop     : DrawLoop[IO, Widget, ExitCode],
   updateLoop   : UpdateLoop[IO, Widget, DownEvent, ExitCode]
): IO[ExitCode] =
  for
    initialWidget <- widgetCell.get
    code <-
      Concurrent[IO]
        .race(
          updateLoop(initialWidget, widgetCell.set, eventSource),
          drawLoop(widgetCell.get)
        )
        .map(_.fold(identity, identity))
  yield code
end applicationLoop
