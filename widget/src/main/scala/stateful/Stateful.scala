package me.katze.gui4s.widget
package stateful

import cats.Monad
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

// TODO Отделить ловку TaskFinished
final case class Stateful[
  // T, Event
  Update[+_, +_] : BiMonad : CatchEvents,
  Merge[+_] : Monad,
  Draw : StatefulDraw, 
  WidgetTask[+_],
  FreeWidgetTree[+_, -_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Merge, Draw, FreeWidgetTree, RaisesEvent, HandlesEvent],
  RaiseableEvent,
  HandleableEvent >: TaskFinished,
  ChildRaiseableEvent,
  ChildHandleableEvent >: HandleableEvent,
](
  name: String,
  state: State[[W] =>> Update[W, RaiseableEvent], Merge, WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
  childTree: PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent],
  runMerge : [T] => Merge[T] => Update[T, Nothing]
)(
  using
    freeWidgetIsMergeable: Mergeable[Merge, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
    freeStatefulFabric   : FreeStatefulFabric[[W] =>> Update[W, RaiseableEvent], Merge, WidgetTask, FreeWidgetTree, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent],
    ChildRaisableEventTypeChecker: RichTypeChecker[ChildRaiseableEvent]
)  extends PlacedWidget[Update, Merge, Draw, FreeWidgetTree, RaiseableEvent, HandleableEvent]:
  import freeWidgetIsMergeable.*

  private type StatefulUpdateResult = Update[FreeWidgetTree[RaiseableEvent, HandleableEvent], RaiseableEvent]
  private type StatefulMergeResult = Merge[FreeWidgetTree[RaiseableEvent, HandleableEvent]]
  private type InternalState = State[[W] =>> Update[W, RaiseableEvent], Merge, WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]

  private def freeStateful(
                            state: InternalState,
                            childTree: FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
                          ) : FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStatefulFabric(this.name, state, childTree)
  end freeStateful

  override def draw: Draw = summon[StatefulDraw[Draw]].drawStateful(this.name, this.state, this.childTree)
  
  override def handleDownEvent(event: HandleableEvent): StatefulUpdateResult =
    event match
      case task : TaskFinished => onTaskFinished(task)
      case otherThing          => handleChildDownEvent(otherThing)
    end match
  end handleDownEvent

  private def onTaskFinished(taskFinished: TaskFinished): StatefulUpdateResult =
    taskFinished match
      case TaskFinished(this.name, Nil, maybeEvent) => reactOnEvent(typeCheckEvent(maybeEvent))
      case TaskFinished(this.name, childName :: furtherPath, eventForChild) => handleChildFinishedTask(childName, furtherPath, eventForChild)
      case _ => handleSomeoneElsesTask()
    end match
  end onTaskFinished
  
  private def typeCheckEvent(maybeEvent : Any) : ChildRaiseableEvent =
    ChildRaisableEventTypeChecker.tryCast(maybeEvent).valueOr(castError => throw Exception(castError))
  end typeCheckEvent
  
  private def handleChildFinishedTask(childWidgetName : String, furtherPath: List[String], eventForChild: Any): StatefulUpdateResult =
    onChildUpdate(this.childTree.handleDownEvent(TaskFinished(childWidgetName, furtherPath, eventForChild)))
  end handleChildFinishedTask
  
  private def handleSomeoneElsesTask(): StatefulUpdateResult =
    noUpdateNeeded()
  end handleSomeoneElsesTask
  
  override def asFree: FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStateful(this.state, this.childTree.asFree)
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): StatefulMergeResult =
    oldState.get(this.name) match
      case Some(value) =>
        for
          newState <- this.state.mergeWithOldState(value)
          mergedChildTree <-  freeWidgetIsMergeable.merge(this.childTree.asFree, newState.render)
        yield freeStateful(newState, mergedChildTree)  
      case None =>
        asFree.pure[Merge]
    end match
  end mergeWithState

  override def childrenStates: Map[String, Any] = Map(this.name -> this.state.state)
  
  private def handleChildDownEvent(otherThing : HandleableEvent): StatefulUpdateResult =
    onChildUpdate(this.childTree.handleDownEvent(otherThing))
  end handleChildDownEvent

  private def onChildUpdate(newChildF: Update[FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent], ChildRaiseableEvent]) : StatefulUpdateResult =
    for
      a <- newChildF.catchEvents
      (newChild, events) = a
      newState <- events.foldLeftM[[T] =>> Update[T, RaiseableEvent], InternalState](this.state)(reactOnEvent)
      res <- runMerge(
        for 
          a <- freeWidgetIsMergeable.merge(this.childTree.asFree, newChild)
          b <- freeWidgetIsMergeable.merge(a, newState.render)
        yield freeStateful(state, b)
      )
    yield res  
  end onChildUpdate
  
  private def reactOnEvent(state : InternalState, event : ChildRaiseableEvent) : Update[InternalState, RaiseableEvent] =
    this.state.handleEvent(event)
  
  private def reactOnEvent(event: ChildRaiseableEvent): StatefulUpdateResult =
    ??? //reactOnEvent(this.state, event)
  end reactOnEvent

  private def noUpdateNeeded() : StatefulUpdateResult =
    asFree.asMonad
  end noUpdateNeeded

  override def filterDeadPaths(currentPath : Path, alive: Set[Path]): Set[Path] =
    val pathToSelf = currentPath.appendFirst(name)
    childTree.filterDeadPaths(pathToSelf, alive.excl(pathToSelf))
  end filterDeadPaths
end Stateful
