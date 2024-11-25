package me.katze.gui4s.example
package recomposition

import task.TaskSet

import cats.{Applicative, Monoid}
import me.katze.gui4s.widget.stateful.{KillTasks, Path}
import cats.*
import cats.syntax.all.{*, given}

trait RecompositionEffect[F, +WidgetTask]:
  def enteredComposition(paths : Set[Path]) : F
  def recomposed(paths : Set[Path], states : Map[String, Any]) : F
  def leftComposition(paths : Set[Path], states : Map[String, Any]) : F
end RecompositionEffect

given recompositionEffect2IsMonoid[F : Monoid, WidgetTask] : Monoid[RecompositionEffect[F, WidgetTask]] with
  override def combine(x: RecompositionEffect[F, WidgetTask], y: RecompositionEffect[F, WidgetTask]): RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(path : Set[Path]): F =
        x.enteredComposition(path) combine y.enteredComposition(path)
      end enteredComposition

      override def leftComposition(path : Set[Path], states : Map[String, Any]): F =
        x.leftComposition(path, states) combine y.leftComposition(path, states)
      end leftComposition

      override def recomposed(path : Set[Path], states : Map[String, Any]): F =
        x.recomposed(path, states) combine y.recomposed(path, states)
      end recomposed
    end new
  end combine

  override def empty: RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(paths : Set[Path]): F =
        Monoid[F].empty
      end enteredComposition

      override def leftComposition(paths : Set[Path], states : Map[String, Any]): F =
        Monoid[F].empty
      end leftComposition

      override def recomposed(paths : Set[Path], states : Map[String, Any]): F =
        Monoid[F].empty
      end recomposed
    end new
  end empty
end recompositionEffect2IsMonoid

given recompositionCanKillTasks[F : Monoid, WidgetTask] : KillTasks[RecompositionEffect[F, WidgetTask]] with
  override def killDetachableTasks(currentPath : Path): RecompositionEffect[F, WidgetTask] =
    new RecompositionEffect[F, WidgetTask]:
      override def enteredComposition(
                                        paths  : Set[Path]
                                      ): F =
        Monoid[F].empty
      end enteredComposition

      override def leftComposition(
                                    paths  : Set[Path],
                                    states : Map[String, Any]
                                  ): F =
        ??? // TODO taskSet.killTask(currentPath)
      end leftComposition

      override def recomposed(
                                paths  : Set[Path],
                                states : Map[String, Any]
                              ): F =
        Monoid[F].empty
      end recomposed
    end new
  end killDetachableTasks
end recompositionCanKillTasks