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
  pathToRoot : Path,
  placedWidget: Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent],
  taskSet : TaskSet[F, WidgetTask]
)(
  using RunPlacement[F, Place]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  type PlacedWidget = Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent]

  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
    val EventResult(newWidget, systemRequests, ios) = placedWidget.handleDownEvent(pathToRoot, event)
    ios.traverse_(taskSet.pushTask) 
      *> EventProcessResult(
          newWidget.runPlacement.map(EventConsumerAdapter(pathToRoot, _, taskSet)),
          systemRequests
        ).pure[F]
  end processEvent
  
  override def draw: Draw =
    placedWidget.draw
  end draw
end EventConsumerAdapter

