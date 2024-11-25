package me.katze.gui4s.example
package recomposition

import task.TaskSet

import cats.Monad
import me.katze.gui4s.widget.{EventResultP, Widget}
import me.katze.gui4s.widget.stateful.{BiMonad, Path}
import cats.*
import cats.syntax.all.{*, given}

def runRecomposition[
  RecomposeEffect : Monoid,
  Update[+_, +_],
  Draw,
  Place[+_],
  WidgetTask,
  UpEvent,
  DownEvent,
](
    pathToRoot : Path,
    oldWidget: Widget[Update, Draw, Place, RecompositionEffect[RecomposeEffect, WidgetTask], UpEvent, DownEvent],
    newWidget: Widget[Update, Draw, Place, RecompositionEffect[RecomposeEffect, WidgetTask], UpEvent, DownEvent]
  ): RecomposeEffect =
  val rec_types = distinctWidgetPaths(oldWidget, newWidget, pathToRoot)
  newWidget.recomposed(pathToRoot).enteredComposition(rec_types.newOnes) |+| 
    newWidget.recomposed(pathToRoot).recomposed(rec_types.recomposed, oldWidget.childrenStates) |+|
    oldWidget.recomposed(pathToRoot).enteredComposition(rec_types.quited) 
end runRecomposition


def distinctWidgetPaths(
                          oldWidgetTree: Widget[?, ?, ?, ?, ?, ?],
                          newWidgetTree: Widget[?, ?, ?, ?, ?, ?],
                          rootPath     : Path
                        ): RecompositionTypes[Set[Path]] =
  distinctPaths(oldWidgetTree.aliveWidgets(rootPath), newWidgetTree.aliveWidgets(rootPath))
end distinctWidgetPaths

final case class RecompositionTypes[+T](newOnes: T, recomposed: T, quited: T)

def distinctPaths(old: Set[Path], alive: Set[Path]): RecompositionTypes[Set[Path]] =
  RecompositionTypes(
    alive -- old,
    alive.intersect(old),
    old -- alive
  )
end distinctPaths