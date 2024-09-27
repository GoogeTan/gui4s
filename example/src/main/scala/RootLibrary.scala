package me.katze.gui4s.example

import draw.*
import place.ApplicationBounds
import root.{RootPlacedWidget, RootWidgetFree, RootWidgetPlaced}
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.impl.WidgetLibraryImpl
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

def runWidget[
  F[+_] : Concurrent,
  Bounds,
  UpEvent,
  DownEvent >: TaskFinished
](
    lib: WidgetLibraryImpl[F, F[Unit], Bounds]
)(
    widget                  : lib.FreeWidget[UpEvent, DownEvent],
    drawLoopExceptionHandler: Throwable => F[Option[ExitCode]],
    api                     : SimpleDrawApi[F]
)(using ProcessRequest[F, UpEvent], ApplicationBounds[F, Bounds]): F[ExitCode] =
  type FreeRootWidget[A, B] = RootWidgetFree[F, F[Unit], lib.WidgetTask[Any], Bounds, lib.FreeWidget, A, B]
  type PlacedRootWidget[A, B] = RootPlacedWidget[F, F[Unit], FreeRootWidget, A, B]
  for
    runningEventsRef <- Ref[F].of(Map[Path, IOOnThread[F]]())
    control <- applicationLoop[F, UpEvent, DownEvent, PlacedRootWidget, FreeRootWidget](
      (eventBus: Queue[F, DownEvent]) => createRootWidget(lib)(widget, IOMasterImpl(runningEventsRef, (a, b) => eventBus.offer(TaskFinished(a, b)))),
      drawLoop[F, Throwable](drawLoopExceptionHandler, api),
      updateLoop[F, PlacedRootWidget[UpEvent, DownEvent], FreeRootWidget[UpEvent, DownEvent], UpEvent, DownEvent]
    )
    code <- control.join
  yield code
end runWidget

def createRootWidget[
  F[+_] : Monad, Draw,
  UpEvent, DownEvent,
  Bounds
](
    lib : WidgetLibraryImpl[F, Draw, Bounds]
)(
    measurable: Placeable[Bounds, PlacedWidget[Draw, lib.WidgetTask[Any], lib.FreeWidget, UpEvent, DownEvent]],
    master : IOMaster[F, lib.WidgetTask[Any]],
)(using place.ApplicationBounds[F, Bounds]) : RootWidgetFree[F, Draw, lib.WidgetTask[Any], Bounds, lib.FreeWidget, UpEvent, DownEvent] =
  RootWidgetFree[F, Draw, lib.WidgetTask[Any], Bounds, lib.FreeWidget, UpEvent, DownEvent](
    measurable,
    master,
    RootWidgetPlaced(_, _, createRootWidget(lib)(_, _)(using summon[Monad[F]]))
  )
end createRootWidget