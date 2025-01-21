package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.{RunnableIO, Task, TaskSet}
import update.{EventConsumer, EventProcessResult}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.stateful.{KillTasks, Path}
import me.katze.gui4s.widget.{EventResult, EventResultP, Widget, foldLeftComposition}

final case class EventConsumerAdapter[
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
  taskSet : TaskSet[F, WidgetTask],
  runRecomposition : Recomposition => F[Unit]
)(
  using RunPlacement[F, Place]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  type PlacedWidget = Widget[EventResultP[WidgetTask], Draw, Place, Recomposition, UpEvent, DownEvent]

  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, Place, Recomposition, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
    val EventResult(newWidget, systemRequests, ios) = placedWidget.handleDownEvent(pathToRoot, event)
    ios.traverse_(taskSet.pushTask) 
      *> EventProcessResult(
          for
            newPlacedWidget <- newWidget.runPlacement
            _ <- runRecomposition(newPlacedWidget.recomposed(pathToRoot, placedWidget.childrenStates))
            _ <- foldLeftComposition[Recomposition](placedWidget.childrenStates, newPlacedWidget.childrenStates).traverse_(runRecomposition)
          yield copy(placedWidget = newPlacedWidget),
          systemRequests
        ).pure[F]
  end processEvent
  
  override def draw: Draw =
    placedWidget.draw
  end draw
end EventConsumerAdapter

