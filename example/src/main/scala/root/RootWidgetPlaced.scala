package me.katze.gui4s.example
package root

import draw.Drawable
import place.RunPlacement
import update.{EventConsumer, EventProcessResult}

import cats.implicits.{*, given}
import cats.syntax.all.{*, given}
import cats.syntax.foldable.given
import cats.{*, given}
import me.katze.gui4s.widget.{PlacedWidget, RunnableIO}
import me.katze.gui4s.widget.stateful.Path
import me.katze.gui4s.widget.placeable.Placeable

type RootPlacedWidget[F[+_], G, FreeWidget[_, _], UpEvent, DownEvent] = EventConsumer[FreeWidget[UpEvent, DownEvent], F, UpEvent, DownEvent] & Drawable[G]

final class RootWidgetPlaced[
  +F[+_] : Monad,
  +G,
  +FreeWidget[+_, -_],
  RootFreeWidget[_, _],
  +WidgetTask,
  UpEvent, DownEvent,
](
    widget:  PlacedWidget[G, WidgetTask, FreeWidget, UpEvent, DownEvent],
    master: IOMaster[F, WidgetTask],
    RootWidgetFree : (FreeWidget[UpEvent, DownEvent], IOMaster[F, WidgetTask]) => RootFreeWidget[UpEvent, DownEvent],
) extends Drawable[G] with EventConsumer[RootFreeWidget[UpEvent, DownEvent], F, UpEvent, DownEvent]:
  override def processEvent(event: DownEvent): F[EventProcessResult[RootFreeWidget[UpEvent, DownEvent], UpEvent]] =
    val res = widget.handleDownEvent(event)
    res.ios.traverse_(master.pushIO) *> EventProcessResult(RootWidgetFree(res.widget, master), res.upEvent).pure[F]
  end processEvent

  override def draw: G = widget.draw
end RootWidgetPlaced
