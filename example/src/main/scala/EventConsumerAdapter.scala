package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.TaskSet
import update.{EventConsumer, EventProcessResult, ProcessRequest}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.{EventResult, Widget}
import me.katze.gui4s.widget.stateful.{KillTasks, Path}

type RecompositionEffect[F[_], WidgetTask] = (TaskSet[F, WidgetTask], Set[Path]) => F[Unit]

given recompositionEffectIsMonoid[F[_] : Applicative, WidgetTask] : Monoid[RecompositionEffect[F, WidgetTask]] with
  override def combine(x: RecompositionEffect[F, WidgetTask], y: RecompositionEffect[F, WidgetTask]): RecompositionEffect[F, WidgetTask] =
    (a, b) => x(a, b) *> y(a, b)
  end combine

  override def empty: RecompositionEffect[F, WidgetTask] =
    (_, _) => ().pure[F]
end recompositionEffectIsMonoid

given recompositionCanKillTasks[F[_] : Applicative, WidgetTask] : KillTasks[RecompositionEffect[F, WidgetTask]] with
  override def killDetachableTasks(currentPath : Path): RecompositionEffect[F, WidgetTask] =
    (taskSet, alive) =>
      if alive.contains(currentPath) then
        ().pure[F]
      else
        taskSet.killTask(currentPath)
      end if
  end killDetachableTasks
end recompositionCanKillTasks

final class EventConsumerAdapter[
  F[+_] : Monad,
  Draw,
  Place[+_],
  WidgetTask,
  UpEvent,
  DownEvent,
](
    placedWidget: Widget[[A, B] =>> EventResult[WidgetTask, A, B], Draw, Place, RecompositionEffect[F, WidgetTask], UpEvent, DownEvent],
    taskSet : TaskSet[F, WidgetTask]
)(
    using ProcessRequest[F, UpEvent], RunPlacement[F, Place]
) extends EventConsumer[F[EventConsumerAdapter[F, Draw, Place, WidgetTask, UpEvent, DownEvent]], F, UpEvent, DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): F[EventProcessResult[F[EventConsumerAdapter[F, Draw, Place, WidgetTask, UpEvent, DownEvent]], UpEvent]] =
    val EventResult(newWidget, systemRequests, ios) = placedWidget.handleDownEvent(event)
    val runIOS = ios.traverse_(taskSet.pushTask)
    val freeWidget =
      for
        newPlacedWidget <- newWidget.runPlacement
        alive = newPlacedWidget.aliveWidgets(Path(List("ROOT")))
        _ <- placedWidget.recomposed(Path(List("ROOT")))(taskSet, alive)
      yield EventConsumerAdapter(newPlacedWidget, taskSet)
    end freeWidget

    runIOS *> EventProcessResult(freeWidget, systemRequests).pure[F]
  end processEvent
  
  override def draw: Draw =
    placedWidget.draw
  end draw
end EventConsumerAdapter
