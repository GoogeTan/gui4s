package me.katze.gui4s.widget
package stateful

import cats.*
import cats.syntax.all.{*, given}

final class TaskResultCatcher[
  +Update[+_, +_] : BiMonad : RaiseEvent,
  +Draw,
  +Place[+_] : FlatMap,
  Recomposition : Semigroup : KillTasks,
  +RaiseableEvent : RichTypeChecker,
  -HandleableEvent >: TaskFinished
](
   name : String,
   nothingToDraw : Draw,
   child : Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent],
) extends Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]:
  override def draw: Draw = nothingToDraw

  override def mergeWithState(pathToParent : Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]] =
    child
      .mergeWithState(pathToParent, oldState)
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end mergeWithState

  override def handleDownEvent(pathToParent : Path, event: HandleableEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    event match
      case TaskFinished(path, newEvent) if path == pathToParent.appendLast(name) =>
        onTaskFinished(pathToParent, newEvent, event)
      case another => child.handleDownEvent(pathToParent, another)
    end match
  end handleDownEvent

  private def onTaskFinished(pathToParent : Path, newEvent: Any, event : HandleableEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    val eventToRaise = summon[RichTypeChecker[RaiseableEvent]].tryCast(newEvent, "Event type mismatch")
    for
      _ <- summon[RaiseEvent[Update]].raise(eventToRaise)
      res <- child.handleDownEvent(pathToParent, event)
    yield res
  end onTaskFinished

  override def aliveWidgets(currentPath: Path): Set[Path] =
    child.aliveWidgets(currentPath)
  end aliveWidgets

  override def asFree: Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]] =
    child
      .asFree
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end asFree

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    child.childrenStates
  end childrenStates

  override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition =
    child.recomposed(currentPath, states) combine summon[KillTasks[Recomposition]].killDetachableTasks(currentPath)
  end recomposed
end TaskResultCatcher
