package me.katze.gui4s.example
package root

import place.ApplicationBounds

import cats.implicits.{*, given}
import cats.syntax.all.given
import cats.syntax.foldable.given
import cats.{*, given}
import me.katze.gui4s.example
import me.katze.gui4s.example.*
import me.katze.gui4s.example.draw.Drawable
import me.katze.gui4s.example.update.{ApplicationRequest, EventConsumer}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.Path


type RootWidgetFreeT[
  F[+_], G,
  WidgetTask,
  Bounds,
  FreeWidget[+_, -_],
] = [UpEvent, DownEvent] =>> RootWidgetFree[F, G, WidgetTask, Bounds, FreeWidget, UpEvent, DownEvent]

final class RootWidgetFree[
  F[+_] : Monad, G, 
  WidgetTask, 
  Bounds, 
  FreeWidget[+_, -_], 
  UpEvent, DownEvent,
](
    child           : Placeable[Bounds, PlacedWidget[G, WidgetTask, FreeWidget, UpEvent, DownEvent]],
    master          : IOMaster[F, WidgetTask],
    RootWidgetPlaced: (
        PlacedWidget[G, WidgetTask, FreeWidget, UpEvent, DownEvent], 
        IOMaster[F, WidgetTask]
      ) => RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, Bounds, FreeWidget], UpEvent, DownEvent]
)(
    using bounds : ApplicationBounds[F, Bounds]
) extends RootPlaceable[F, RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, Bounds, FreeWidget], UpEvent, DownEvent]]:
  
  override def place(): F[RootPlacedWidget[F, G, RootWidgetFreeT[F, G, WidgetTask, Bounds, FreeWidget], UpEvent, DownEvent]] =
    for
      bounds <- bounds.currentBounds
      placed =  child.place(bounds)
      _ <- killDeadIOS(placed)
    yield RootWidgetPlaced(placed, master)
  end place

  private def killDeadIOS(newWidget : PlacedWidget[?, ?, ?, ?, ?]) : F[Unit] =
    for
      alive  <- master.alive
      dead   =  newWidget.filterDeadPaths(Path(List("ROOT")), alive) /// TODO Проверить, что "ROOT" - хорошая идея.
      _      <- dead.toList.traverse_(master.detach)
    yield ()
  end killDeadIOS
end RootWidgetFree
