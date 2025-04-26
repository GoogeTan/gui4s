package me.katze.gui4s.widget
package stateful

import cats.*
import cats.syntax.all.*

final class TaskResultCatcher[
  +Update[+_] : {Monad},
  +Draw,
  +Place[+_] : FlatMap,
  Recomposition,
  -HandleableEvent >: TaskFinished
](
   name : String,
   child : Widget[Update, Draw, Place, Recomposition, HandleableEvent],
)(
 using RE : RaiseEvent[Update[Unit]]
) extends Widget[Update, Draw, Place, Recomposition, HandleableEvent]:
  override def draw: Draw = child.draw 

  override def mergeWithState(pathToParent : Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]] =
    child
      .mergeWithState(pathToParent, oldState)
      .map(TaskResultCatcher(name, _))
  end mergeWithState

  override def handleDownEvent(pathToParent : Path, event: HandleableEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]]] =
    event match
      case TaskFinished(path, newEvent) if path == pathToParent.appendLast(name) =>
        onTaskFinished(pathToParent, newEvent, event)
      case another => child.handleDownEvent(pathToParent, another)
    end match
  end handleDownEvent

  private def onTaskFinished(pathToParent : Path, newEvent: Any, event : HandleableEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]]] =
    RE.raise(newEvent) *> child.handleDownEvent(pathToParent, event)
  end onTaskFinished

  override def asUnplaced: Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]] =
    child
      .asUnplaced
      .map(TaskResultCatcher(name, _))
  end asUnplaced

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    child.childrenStates
  end childrenStates

  override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition =
    child.recomposed(currentPath, states) // TODO kill tasks on detach |+| KT.killDetachableTasks(currentPath)
  end recomposed
end TaskResultCatcher
