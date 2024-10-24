package me.katze.gui4s.widget
package stateful

import cats.*
import cats.syntax.all.{*, given}

final class TaskResultCatcher[
  +Update[+_, +_] : BiMonad : RaiseEvent,
  +Draw,
  +Place[+_] : FlatMap,
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Draw, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  +RaiseableEvent : RichTypeChecker,
  -HandleableEvent >: TaskFinished
](
  name : String,
  nothingToDraw : Draw,
  child : PlacedWidget[Update, Draw, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent],
  constructRealWidget : PlacedWidget[Update, Draw, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent] => PlacedWidgetTree[RaiseableEvent, HandleableEvent]
) extends PlacedWidget[Update, Draw, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent]:
  override def draw: Draw = nothingToDraw

  override def mergeWithState(oldState: Map[String, Any]): Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]] =
    child
      .mergeWithState(oldState)
      .map(TaskResultCatcher(name, nothingToDraw, _, constructRealWidget))
      .map(constructRealWidget)
  end mergeWithState

  override def handleDownEvent(event: HandleableEvent): Update[Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    event match
      case TaskFinished(this.name, Nil, newEvent) => onTaskFinished(newEvent, event)
      case TaskFinished(this.name, childName :: furtherPath, eventForChild) => handleChildFinishedTask(childName, furtherPath, eventForChild)
      case another                                => child.handleDownEvent(another)
    end match
  end handleDownEvent

  private def handleChildFinishedTask(childWidgetName : String, furtherPath: List[String], eventForChild: Any) : Update[Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]], RaiseableEvent] =
    for
      freeChild : Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]] <- child.handleDownEvent(TaskFinished(childWidgetName, furtherPath, eventForChild))
      freeCatcher = freeChild.map(
        a =>
          constructRealWidget(
            TaskResultCatcher[Update, Draw, Place, PlacedWidgetTree, RaiseableEvent, HandleableEvent](name, nothingToDraw, a, constructRealWidget)
          )
      )
    yield freeCatcher
  end handleChildFinishedTask

  private def onTaskFinished(newEvent: Any, event : HandleableEvent): Update[Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]], RaiseableEvent] =
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

  override def asFree: Place[PlacedWidgetTree[RaiseableEvent, HandleableEvent]] =
    child
      .asFree
      .map(TaskResultCatcher(name, nothingToDraw, _, constructRealWidget))
      .map(constructRealWidget)
  end asFree

  override def childrenStates: Map[String, Any] =
    child.childrenStates
  end childrenStates
end TaskResultCatcher
