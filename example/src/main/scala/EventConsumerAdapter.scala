package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.TaskSet
import update.{EventConsumer, EventProcessResult, ProcessRequest}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.{EventResult, PlacedWidget}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.stateful.Path


class EventConsumerAdapter[
  F[+_] : Monad,
  Draw,
  PlacementEffect[+_],
  WidgetTask[+_],
  UpEvent,
  DownEvent,
](
  using lib: WidgetLibraryImpl[F, Draw, PlacementEffect, WidgetTask, DownEvent]
)(
    placedWidget: lib.PlacedWidget[UpEvent, DownEvent],
    taskSet : TaskSet[F, WidgetTask[Any]]
)(
    using ProcessRequest[F, UpEvent], RunPlacement[F, PlacementEffect]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, PlacementEffect, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, PlacementEffect, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
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
