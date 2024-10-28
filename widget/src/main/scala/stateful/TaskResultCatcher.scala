package me.katze.gui4s.widget
package stateful

import cats.*
import cats.syntax.all.{*, given}

final class TaskResultCatcher[
  +Update[+_, +_] : BiMonad : RaiseEvent,
  +Draw,
  +Place[+_] : FlatMap,
  +RaiseableEvent : RichTypeChecker,
  -HandleableEvent >: TaskFinished
](
  name : String,
  nothingToDraw : Draw,
  child : PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent],
) extends PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]:
  override def draw: Draw = nothingToDraw

  override def mergeWithState(oldState: Map[String, Any]): Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]] =
    child
      .mergeWithState(oldState)
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end mergeWithState

  override def handleDownEvent(event: HandleableEvent): Update[Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    event match
      case TaskFinished(this.name, Nil, newEvent) => onTaskFinished(newEvent, event)
      case TaskFinished(this.name, childName :: furtherPath, eventForChild) => handleChildFinishedTask(childName, furtherPath, eventForChild)
      case another                                => child.handleDownEvent(another)
    end match
  end handleDownEvent

  private def handleChildFinishedTask(childWidgetName : String, furtherPath: List[String], eventForChild: Any) : Update[Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    for
      freeChild : Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]] <- child.handleDownEvent(TaskFinished(childWidgetName, furtherPath, eventForChild))
      freeCatcher = freeChild.map(TaskResultCatcher(name, nothingToDraw, _))
    yield freeCatcher
  end handleChildFinishedTask

  private def onTaskFinished(newEvent: Any, event : HandleableEvent): Update[Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    val eventToRaise = summon[RichTypeChecker[RaiseableEvent]]
      .tryCast(newEvent)
      .fold(a => throw Exception(a), a => a)
    for
      _ <- summon[RaiseEvent[Update]].raise(eventToRaise)
      res <- child.handleDownEvent(event)
    yield res
  end onTaskFinished

  override def filterDeadPaths(
                                currentPath: Path,
                                alive      : Set[Path]
                              ): Set[Path] =
    child.filterDeadPaths(currentPath, alive)
  end filterDeadPaths

  override def asFree: Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]] =
    child
      .asFree
      .map(TaskResultCatcher(name, nothingToDraw, _))
  end asFree

  override def childrenStates: Map[String, Any] =
    child.childrenStates
  end childrenStates
end TaskResultCatcher
