package me.katze.gui4s.example

import draw.*
import place.RunPlacement
import root.{RootPlacedWidget, RootWidgetFree, RootWidgetPlaced}
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.Measurable
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

// TODO отрефакторить это сверху вниз по-человечески. 
def runWidget[
  F[+_] : Concurrent,
  Draw,
  MU,
  PlacementEffect[+_],
  UpEvent,
  DownEvent >: TaskFinished
](
    lib: WidgetLibraryImpl[F, Draw, PlacementEffect]
)(
    widget                  : lib.FreeWidget[UpEvent, DownEvent],
    drawLoopExceptionHandler: Throwable => F[Option[ExitCode]],
    api : SimpleDrawApi[MU, Draw],
    runDraw : Draw => F[Unit]
)(using ProcessRequest[F, UpEvent], RunPlacement[F, PlacementEffect]): F[ExitCode] =
  type FreeRootWidget[A, B] = RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, A, B]
  type PlacedRootWidget[A, B] = RootPlacedWidget[F, Draw, FreeRootWidget, A, B]
  for
    runningEventsRef <- Ref[F].of(Map[Path, IOOnThread[F]]())
    control <- applicationLoop[F, UpEvent, DownEvent, PlacedRootWidget, FreeRootWidget](
      (eventBus: Queue[F, DownEvent]) => createRootWidget(lib)(widget, IOMasterImpl(runningEventsRef, (a, b) => eventBus.offer(TaskFinished(a, b)))),
      currentWidget => drawLoop[F, Throwable](drawLoopExceptionHandler, runDraw(api.beginDraw), runDraw(api.endDraw))(currentWidget.map(_.draw).flatMap(runDraw)),
      updateLoop[F, PlacedRootWidget[UpEvent, DownEvent], FreeRootWidget[UpEvent, DownEvent], UpEvent, DownEvent]
    )
    code <- control.join
  yield code
end runWidget

def createRootWidget[
  F[+_] : Monad, Draw, PlacementEffect[+_],
  UpEvent, DownEvent
](
    lib : WidgetLibraryImpl[F, Draw, PlacementEffect]
)(
    measurable: lib.FreeWidget[UpEvent, DownEvent],
    master : IOMaster[F, lib.WidgetTask[Any]],
)(using place.RunPlacement[F, PlacementEffect]) : RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, UpEvent, DownEvent] =
  RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, UpEvent, DownEvent](
    measurable,
    master,
    RootWidgetPlaced(_, _, createRootWidget(lib)(_, _)(using summon[Monad[F]]))
  )
end createRootWidget