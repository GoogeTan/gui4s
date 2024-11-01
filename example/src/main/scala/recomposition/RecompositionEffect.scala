package me.katze.gui4s.example
package recomposition

import task.TaskSet

import cats.{Applicative, Monoid}
import me.katze.gui4s.widget.stateful.{KillTasks, Path}
import cats.*
import cats.syntax.all.{*, given}

trait RecompositionEffect[F[_], +WidgetTask]:
  def enteredComposition(taskSet : TaskSet[F, WidgetTask], paths : Set[Path]) : F[Unit]
  def recomposed(taskSet : TaskSet[F, WidgetTask], paths : Set[Path]) : F[Unit]
  def leftComposition(taskSet : TaskSet[F, WidgetTask], paths : Set[Path]) : F[Unit]
end RecompositionEffect

given recompositionEffect2IsMonoid[F[_] : Applicative, WidgetTask] : Monoid[RecompositionEffect[F, WidgetTask]] with
  override def combine(x: RecompositionEffect[F, WidgetTask], y: RecompositionEffect[F, WidgetTask]): RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(taskSet: TaskSet[F, WidgetTask], path : Set[Path]): F[Unit] =
        x.enteredComposition(taskSet, path) *> y.enteredComposition(taskSet, path)
      end enteredComposition

      override def leftComposition(taskSet: TaskSet[F, WidgetTask], path : Set[Path]): F[Unit] =
        x.leftComposition(taskSet, path) *> y.leftComposition(taskSet, path)
      end leftComposition

      override def recomposed(taskSet: TaskSet[F, WidgetTask], path : Set[Path]): F[Unit] =
        x.recomposed(taskSet, path) *> y.recomposed(taskSet, path)
      end recomposed
    end new
  end combine

  override def empty: RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(taskSet: TaskSet[F, WidgetTask], paths  : Set[Path]): F[Unit] =
        ().pure[F]
      end enteredComposition

      override def leftComposition(taskSet: TaskSet[F, WidgetTask], paths  : Set[Path]): F[Unit] =
        ().pure[F]
      end leftComposition

      override def recomposed(taskSet: TaskSet[F, WidgetTask], paths  : Set[Path]): F[Unit] =
        ().pure[F]
      end recomposed
    end new
  end empty
end recompositionEffect2IsMonoid

given recompositionCanKillTasks[F[_] : Applicative, WidgetTask] : KillTasks[RecompositionEffect[F, WidgetTask]] with
  override def killDetachableTasks(currentPath : Path): RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(
                                       taskSet: TaskSet[F, WidgetTask],
                                       paths  : Set[Path]
                                     ): F[Unit] =
        ().pure[F]
      end enteredComposition

      override def leftComposition(
                                    taskSet: TaskSet[F, WidgetTask],
                                    paths  : Set[Path]
                                  ): F[Unit] =
        taskSet.killTask(currentPath)
      end leftComposition

      override def recomposed(
                               taskSet: TaskSet[F, WidgetTask],
                               paths  : Set[Path]
                             ): F[Unit] =
        ().pure[F]
      end recomposed
    end new
  end killDetachableTasks
end recompositionCanKillTasks