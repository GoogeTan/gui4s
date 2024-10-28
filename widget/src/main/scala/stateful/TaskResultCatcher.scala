package me.katze.gui4s.widget
package stateful

import cats.*
import cats.syntax.all.{*, given}

final class TaskResultCatcher[
  +Update[+_, +_] : BiMonad : RaiseEvent,
  +Draw,
  +Place[+_] : FlatMap,
  LeftComposition : Semigroup : KillTasks,
  +RaiseableEvent : RichTypeChecker,
  -HandleableEvent >: TaskFinished
](
    name : String,
    nothingToDraw : Draw,
    child : Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent],
) extends Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]:
  override def draw: Draw = nothingToDraw

  override def mergeWithState(oldState: Map[String, Any]): Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]] =
    child
      .mergeWithState(oldState)
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end mergeWithState

  override def handleDownEvent(event: HandleableEvent): Update[Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    event match
      case TaskFinished(this.name, Nil, newEvent) => onTaskFinished(newEvent, event)
      case TaskFinished(this.name, childName :: furtherPath, eventForChild) => handleChildFinishedTask(childName, furtherPath, eventForChild)
      case another                                => child.handleDownEvent(another)
    end match
  end handleDownEvent

  private def handleChildFinishedTask(childWidgetName : String, furtherPath: List[String], eventForChild: Any) : Update[Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    for
      freeChild : Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]] <- child.handleDownEvent(TaskFinished(childWidgetName, furtherPath, eventForChild))
      freeCatcher = freeChild.map(TaskResultCatcher(name, nothingToDraw, _))
    yield freeCatcher
  end handleChildFinishedTask

  private def onTaskFinished(newEvent: Any, event : HandleableEvent): Update[Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    val eventToRaise = summon[RichTypeChecker[RaiseableEvent]]
      .tryCast(newEvent)
      .fold(a => throw Exception(a), a => a)
    for
      _ <- summon[RaiseEvent[Update]].raise(eventToRaise)
      res <- child.handleDownEvent(event)
    yield res
  end onTaskFinished

  override def aliveWidgets(currentPath: Path): Set[Path] =
    child.aliveWidgets(currentPath)
  end aliveWidgets

  override def asFree: Place[Widget[Update, Draw, Place, LeftComposition, RaiseableEvent, HandleableEvent]] =
    child
      .asFree
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end asFree

  override def childrenStates: Map[String, Any] =
    child.childrenStates
  end childrenStates

  override def recomposed(currentPath : Path): LeftComposition =
    child.recomposed(currentPath) combine summon[KillTasks[LeftComposition]].killDetachableTasks(currentPath)
  end recomposed
end TaskResultCatcher
