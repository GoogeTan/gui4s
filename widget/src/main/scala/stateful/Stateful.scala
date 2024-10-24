package me.katze.gui4s.widget
package stateful

import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

final case class Stateful[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw : StatefulDraw, 
  FreeWidgetTree[+_, -_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Draw, FreeWidgetTree, RaisesEvent, HandlesEvent],
  RaiseableEvent,
  HandleableEvent,
  ChildRaiseableEvent,
  ChildHandleableEvent >: HandleableEvent,
](
  name: String,
  state: State[[W] =>> Update[W, RaiseableEvent], ChildRaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
  childTree: PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent],
)(
  using
    freeWidgetIsMergeable: Mergeable[FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
    freeStatefulFabric   : FreeStatefulFabric[[W] =>> Update[W, RaiseableEvent], FreeWidgetTree, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent]
)  extends PlacedWidget[Update, Draw, FreeWidgetTree, RaiseableEvent, HandleableEvent]:
  private type StatefulUpdateResult = Update[FreeWidgetTree[RaiseableEvent, HandleableEvent], RaiseableEvent]
  private type InternalState = State[[W] =>> Update[W, RaiseableEvent], ChildRaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]

  private def freeStateful(
                            state: InternalState,
                            childTree: FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
                          ) : FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStatefulFabric(this.name, state, childTree)
  end freeStateful

  override def draw: Draw = 
    summon[StatefulDraw[Draw]].drawStateful(this.name, this.state, this.childTree)
  end draw
  
  override def handleDownEvent(event: HandleableEvent): StatefulUpdateResult =
    onChildUpdate(this.childTree.handleDownEvent(event))
  end handleDownEvent

  override def asFree: FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStateful(this.state, this.childTree.asFree)
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    oldState.get(this.name) match
      case Some(value) =>
        val newState = this.state.mergeWithOldState(value)
        val mergedChildTree =  freeWidgetIsMergeable.merge(this.childTree.asFree, newState.render)
        freeStateful(newState, mergedChildTree)  
      case None =>
        asFree
    end match
  end mergeWithState

  override def childrenStates: Map[String, Any] = 
    Map(this.name -> this.state.state)
  end childrenStates

  private def onChildUpdate(newChildF: Update[FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent], ChildRaiseableEvent]) : StatefulUpdateResult =
    for
      tmp <- newChildF.catchEvents
      (newChild, events) = tmp
      newState <- events.foldLeftM[[T] =>> Update[T, RaiseableEvent], InternalState](this.state)(_.handleEvent(_))
      res = freeStateful(state, freeWidgetIsMergeable.merge(freeWidgetIsMergeable.merge(this.childTree.asFree, newChild), newState.render))
    yield res  
  end onChildUpdate

  override def filterDeadPaths(currentPath : Path, alive: Set[Path]): Set[Path] =
    val pathToSelf = currentPath.appendFirst(name)
    childTree.filterDeadPaths(pathToSelf, alive.excl(pathToSelf))
  end filterDeadPaths
end Stateful
