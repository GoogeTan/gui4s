package me.katze.gui4s.example
package recomposition

import task.TaskSet

import cats.Monad
import me.katze.gui4s.widget.{EventResultP, Widget}
import me.katze.gui4s.widget.stateful.Path
import cats.*
import cats.syntax.all.{*, given}

final class RunRecompositionImpl[
  F[+_] : Applicative,
  Draw,
  Place[+_],
  WidgetTask,
  UpEvent,
  DownEvent,
](
    pathToRoot : Path
) extends RunRecomposition[TaskSet[F, WidgetTask], F[Unit], Widget[EventResultP[WidgetTask], Draw, Place, RecompositionEffect[F, WidgetTask], UpEvent, DownEvent]]:
  override def run(
                    taskSet : TaskSet[F, WidgetTask],
                    oldWidget: Widget[EventResultP[WidgetTask], Draw, Place, RecompositionEffect[F, WidgetTask], UpEvent, DownEvent],
                    newWidget: Widget[EventResultP[WidgetTask], Draw, Place, RecompositionEffect[F, WidgetTask], UpEvent, DownEvent]
                  ): F[Unit] =
    val rec_types = distinctWidgetPaths(oldWidget, newWidget, pathToRoot)
    newWidget.recomposed(pathToRoot).enteredComposition(taskSet, rec_types.newOnes)
      *> newWidget.recomposed(pathToRoot).recomposed(taskSet, rec_types.recomposed)
      *> oldWidget.recomposed(pathToRoot).enteredComposition(taskSet, rec_types.quited)
  end run


  def distinctWidgetPaths(
                            oldWidgetTree: Widget[?, ?, ?, ?, ?, ?],
                            newWidgetTree: Widget[?, ?, ?, ?, ?, ?],
                            rootPath     : Path
                          ): RecompositionTypes[Set[Path]] =
    distinctPaths(oldWidgetTree.aliveWidgets(rootPath), newWidgetTree.aliveWidgets(rootPath))
  end distinctWidgetPaths

  final case class RecompositionTypes[T](newOnes: T, recomposed: T, quited: T)

  def distinctPaths(old: Set[Path], alive: Set[Path]): RecompositionTypes[Set[Path]] =
    RecompositionTypes(
      alive -- old,
      alive.intersect(old),
      old -- alive
    )
  end distinctPaths
end RunRecompositionImpl