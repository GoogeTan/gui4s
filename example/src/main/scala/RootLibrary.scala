package me.katze.gui4s.example

import draw.*
import place.RunPlacement
import root.{RootPlacedWidget, RootWidgetFree, RootWidgetPlaced}
import task.TaskSet
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.stateful.TaskFinished

// TODO отрефакторить это сверху вниз по-человечески. 
def runWidget[
  F[+_] : Concurrent,
  Draw,
  MU,
  PlacementEffect[+_],
  UpEvent,
  DownEvent >: TaskFinished,
](
    lib: WidgetLibraryImpl[F, Draw, PlacementEffect, DownEvent]
)(
    widget                  : Queue[F, DownEvent] => RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, UpEvent, DownEvent],
    drawLoopExceptionHandler: Throwable => F[Option[ExitCode]],
    api                     : SimpleDrawApi[MU, Draw],
    runDraw                 : Draw => F[Unit]
)(using ProcessRequest[F, UpEvent]): F[ExitCode] =
  type FreeRootWidget[A, B] = RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, A, B]
  type PlacedRootWidget[A, B] = RootPlacedWidget[F, Draw, FreeRootWidget, A, B]
  applicationLoop[F, UpEvent, DownEvent, PlacedRootWidget, FreeRootWidget](
    widget,
    currentWidget => drawLoop[F, Throwable](drawLoopExceptionHandler, runDraw(api.beginDraw), runDraw(api.endDraw))(currentWidget.map(_.draw).flatMap(runDraw)),
    updateLoop[F, PlacedRootWidget[UpEvent, DownEvent], FreeRootWidget[UpEvent, DownEvent], UpEvent, DownEvent]
  ).flatMap(_.join)
end runWidget

def createRootWidget[
  F[+_] : Monad, Draw, PlacementEffect[+_],
  UpEvent, DownEvent >: TaskFinished
](
    lib : WidgetLibraryImpl[F, Draw, PlacementEffect, DownEvent]
)(
    freeWidget : lib.FreeWidget[UpEvent, DownEvent],
    taskSet   : TaskSet[F, lib.WidgetTask[Any]],
)(using place.RunPlacement[F, PlacementEffect]) : RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, UpEvent, DownEvent] =
  RootWidgetFree[F, Draw, lib.WidgetTask[Any], PlacementEffect, lib.FreeWidget, UpEvent, DownEvent](
    freeWidget,
    taskSet,
    RootWidgetPlaced(_, _, createRootWidget(lib)(_, _)(using summon[Monad[F]]))
  )
end createRootWidget