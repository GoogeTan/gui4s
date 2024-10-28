package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.TaskSet
import update.{EventConsumer, EventProcessResult, ProcessRequest}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.{EventResult, PlacedWidget}
import me.katze.gui4s.widget.stateful.Path


final class EventConsumerAdapter[
  F[+_] : Monad,
  Draw,
  Place[+_],
  WidgetTask,
  UpEvent,
  DownEvent,
](
    placedWidget: PlacedWidget[[A, B] =>> EventResult[WidgetTask, A, B], Draw, Place, UpEvent, DownEvent],
    taskSet : TaskSet[F, WidgetTask]
)(
    using ProcessRequest[F, UpEvent], RunPlacement[F, Place]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, Place, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, Place, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
    val EventResult(newWidget, systemRequests, ios) = placedWidget.handleDownEvent(event)
    val runIOS = ios.traverse_(taskSet.pushTask)
    val freeWidget = newWidget.runPlacement.map(EventConsumerAdapter(_, taskSet))
    runIOS *> EventProcessResult(freeWidget, systemRequests).pure[F]
  end processEvent
  
  override def draw: Draw =
    placedWidget.draw
  end draw
end EventConsumerAdapter

private def killDeadIOS[F[_] : Monad, T](taskSet : TaskSet[F, T], newWidget: PlacedWidget[?, ?, ?, ?, ?]): F[Unit] =
  for
    alive <- taskSet.aliveTasksPaths
    dead = newWidget.filterDeadPaths(Path(List("ROOT")), alive) /// TODO Проверить, что "ROOT" - хорошая идея.
    _ <- dead.toList.traverse_(taskSet.killTask)
  yield ()
end killDeadIOS
