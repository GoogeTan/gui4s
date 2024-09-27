package me.katze.gui4s.example


import draw.Drawable
import place.ApplicationBounds
import update.{EventConsumer, EventProcessResult}

import cats.implicits.{*, given}
import cats.syntax.all.{*, given}
import cats.syntax.foldable.given
import cats.{*, given}
import me.katze.gui4s.widget.PlacedWidget
import me.katze.gui4s.widget.stateful.Path
import me.katze.gui4s.widget.placeable.Placeable


trait TruePlacedWidget[
  +G,
  +WidgetTask,
  -Bounds,
  +UpEvent, -DownEvent,
] extends PlacedWidget[G, WidgetTask, [A, B] =>> Placeable[Bounds, TruePlacedWidget[G, WidgetTask, Bounds, A, B]], UpEvent, DownEvent]


final class RootWidgetPlaced[
  F[+_] : Monad,
  +G,
  -Bounds, 
  +WidgetTask,
  +UpEvent, -DownEvent,
](
  widget : TruePlacedWidget[G, WidgetTask, Bounds, DownEvent, UpEvent],
  master : IOMaster[F]
)(using ApplicationBounds[F, Bounds]) extends Drawable[G] with EventConsumer[RootWidgetFree[F, G, Bounds, WidgetTask, UpEvent, DownEvent], F, DownEvent, UpEvent]:

  override def processEvent(event: DownEvent): F[EventProcessResult[RootWidgetFree[F, G, Bounds, WidgetTask, UpEvent, DownEvent], UpEvent]] =
    val res = widget.handleDownEvent(event)
    pushIOS(res.ios) *> EventProcessResult(RootWidgetFree(res.widget, master), res.upEvent).pure[F]
  end processEvent

  private def pushIOS(ios : List[RunnableIO[WidgetTask]]) : F[Unit] =
    ios.traverse_(io => master.pushIO(io.io, io.path, io.keepAliveAfterWidgetDetach))
  end pushIOS

  override def draw: G = widget.draw
end RootWidgetPlaced


final class RootWidgetFree[
  F[+_] : Monad, +G, -Bounds, +WidgetTask,  +UpEvent, -DownEvent,
](
    measurable: Placeable[Bounds, TruePlacedWidget[G, WidgetTask, Bounds, UpEvent, DownEvent]],
    master : IOMaster[F]
)(
    using bounds : ApplicationBounds[F, Bounds]
) extends me.katze.gui4s.example.Placeable[F, RootWidgetPlaced[F, G, Bounds, WidgetTask, UpEvent, DownEvent]]:
  override def place(): F[RootWidgetPlaced[F, G, Bounds, WidgetTask, UpEvent, DownEvent]] =
    for
      bounds <- bounds.currentBounds
      placed =  measurable.place(bounds)
      _ <- killDeadIOS(placed)
    yield RootWidgetPlaced(placed, master)
  end place

  private def killDeadIOS(newWidget : TruePlacedWidget[G, WidgetTask, Bounds, UpEvent, DownEvent]) : F[Unit] =
    for
      alive  <- master.alive
      dead   =  newWidget.filterDeadPaths(alive)
      _      <- dead.toList.traverse_(master.detach)
    yield ()
  end killDeadIOS
end RootWidgetFree
