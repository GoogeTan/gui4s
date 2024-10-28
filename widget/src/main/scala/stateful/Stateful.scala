package me.katze.gui4s.widget
package stateful

import cats.FlatMap
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

final case class Stateful[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw : StatefulDraw, 
  Place[+_] : FlatMap,
  RaiseableEvent,
  HandleableEvent,
  ChildRaiseableEvent,
  ChildHandleableEvent >: HandleableEvent,
](
   name: String,
   state: State[[W] =>> Update[W, RaiseableEvent], ChildRaiseableEvent, Place[Widget[Update, Draw, Place, ChildRaiseableEvent, ChildHandleableEvent]]],
   childTree: Widget[Update, Draw, Place, ChildRaiseableEvent, ChildHandleableEvent],
)  extends Widget[Update, Draw, Place, RaiseableEvent, HandleableEvent]:
  private type FreeWidgetTree[+A, -B] = Place[Widget[Update, Draw, Place, A, B]]
  private type StatefulUpdateResult = Update[Place[Widget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent]
  private type InternalState = State[[WW] =>> Update[WW, RaiseableEvent], ChildRaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]

  private def freeStateful(
                            state: InternalState,
                            childTree: Place[Widget[Update, Draw, Place, ChildRaiseableEvent, ChildHandleableEvent]]
                          ) : FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    childTree.map(Stateful(name, state, _))
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
        val mergedChildTree =  mergeFreeTrees(this.childTree.asFree, newState.render)
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
      res = freeStateful(state, mergeFreeTrees(mergeFreeTrees(this.childTree.asFree, newChild), newState.render))
    yield res  
  end onChildUpdate

  override def filterDeadPaths(currentPath : Path, alive: Set[Path]): Set[Path] =
    val pathToSelf = currentPath.appendFirst(name)
    childTree.filterDeadPaths(pathToSelf, alive.excl(pathToSelf))
  end filterDeadPaths
  
  private def mergeFreeTrees[A, B](oldOne: FreeWidgetTree[A, B], newOne: FreeWidgetTree[A, B]) =
    FlatMap[Place].flatMap2(oldOne, newOne)((a, b) => b.mergeWithState(a.childrenStates))
  end mergeFreeTrees
end Stateful
