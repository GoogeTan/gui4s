package me.katze.gui4s.example
package root

import place.RunPlacement

import cats.implicits.{*, given}
import cats.syntax.all.given
import cats.syntax.foldable.given
import cats.{*, given}
import me.*
import me.katze.gui4s.example
import task.TaskSet
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.stateful.Path


type RootWidgetFreeT[
  F[+_], G,
  WidgetTask,
  PlacementEffect[_],
  FreeWidget[+_, -_],
] = [UpEvent, DownEvent] =>> RootWidgetFree[F, G, WidgetTask, PlacementEffect, FreeWidget, UpEvent, DownEvent]

final class RootWidgetFree[
  F[+_] : Monad, G, 
  WidgetTask, 
  PlacementEffect[_],
  FreeWidget[+_, -_], 
  UpEvent, DownEvent,
](
    child           : PlacementEffect[PlacedWidget[G, WidgetTask, FreeWidget, UpEvent, DownEvent]],
    master          : TaskSet[F, WidgetTask],
    RootWidgetPlaced: (
        PlacedWidget[G, WidgetTask, FreeWidget, UpEvent, DownEvent], 
        TaskSet[F, WidgetTask]
      ) => RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, PlacementEffect, FreeWidget], UpEvent, DownEvent]
)(
    using bounds : RunPlacement[F, PlacementEffect]
) extends RootPlaceable[F, RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, PlacementEffect, FreeWidget], UpEvent, DownEvent]]:
  
  override def place(): F[RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, PlacementEffect, FreeWidget], UpEvent, DownEvent]] =
    for
      placed <- bounds.run(child)
      _ <- killDeadIOS(placed)
    yield RootWidgetPlaced(placed, master)
  end place

  private def killDeadIOS(newWidget : PlacedWidget[?, ?, ?, ?, ?]) : F[Unit] =
    for
      alive  <- master.aliveTasksPaths
      dead   =  newWidget.filterDeadPaths(Path(List("ROOT")), alive) /// TODO Проверить, что "ROOT" - хорошая идея.
      _      <- dead.toList.traverse_(master.killTask)
    yield ()
  end killDeadIOS
end RootWidgetFree
