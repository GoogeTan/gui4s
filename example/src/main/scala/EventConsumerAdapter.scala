package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.TaskSet
import update.{EventConsumer, EventProcessResult, ProcessRequest}
import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.{EventResult, EventResultP, Widget}
import me.katze.gui4s.widget.stateful.{KillTasks, Path}

final class EventConsumerAdapter[
  F[+_] : Monad,
  Draw,
  Place[+_],
  Recomposition,
  WidgetTask,
  UpEvent,
  DownEvent,
](
  placedWidget: Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent],
  taskSet : TaskSet[F, WidgetTask]
)(
    using
      rp : RunPlacement[F, Place],
      rr : RunRecomposition[TaskSet[F, WidgetTask], F[Unit], Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent]]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  type PlacedWidget = Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent]

  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
    val EventResult(newWidget, systemRequests, ios) = placedWidget.handleDownEvent(event)
    ios.traverse_(taskSet.pushTask) 
      *> EventProcessResult(
          updateWidgetFromOld(newWidget).map(EventConsumerAdapter(_, taskSet)),
          systemRequests
        ).pure[F]
  end processEvent

  def updateWidgetFromOld(newWidget: Place[PlacedWidget]): F[PlacedWidget] =
    for
      newPlacedWidget <- newWidget.runPlacement
      _ <- rr.run(taskSet, placedWidget, newPlacedWidget)
    yield newPlacedWidget
  end updateWidgetFromOld
  
  override def draw: Draw =
    placedWidget.draw
  end draw
end EventConsumerAdapter

