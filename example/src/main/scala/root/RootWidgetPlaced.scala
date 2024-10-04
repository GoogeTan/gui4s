package me.katze.gui4s.example
package root

import draw.Drawable
import update.{EventConsumer, EventProcessResult}
import cats.syntax.all.{*, given}
import cats.*
import task.TaskSet
import me.katze.gui4s.widget.{PlacedWidget, RunnableIO}

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
    master: TaskSet[F, WidgetTask],
    RootWidgetFree : (FreeWidget[UpEvent, DownEvent], TaskSet[F, WidgetTask]) => RootFreeWidget[UpEvent, DownEvent],
) extends Drawable[G] with EventConsumer[RootFreeWidget[UpEvent, DownEvent], F, UpEvent, DownEvent]:
  override def processEvent(event: DownEvent): F[EventProcessResult[RootFreeWidget[UpEvent, DownEvent], UpEvent]] =
    val res = widget.handleDownEvent(event)
    res.ios.traverse_(master.pushTask) *> EventProcessResult(RootWidgetFree(res.widget, master), res.upEvent).pure[F]
  end processEvent

  override def draw: G = widget.draw
end RootWidgetPlaced
