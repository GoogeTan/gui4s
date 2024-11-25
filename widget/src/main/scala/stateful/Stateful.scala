package me.katze.gui4s.widget
package stateful

import cats.FlatMap
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

type RecompositionRunner[Update[+_, +_], Draw, Place[+_], Recomposition, HandleableEvent] = [A] => (
  Widget[Update, Draw, Place, Recomposition, A, HandleableEvent],
    Widget[Update, Draw, Place, Recomposition, A, HandleableEvent]
  ) => Place[Widget[Update, Draw, Place, Recomposition, A, HandleableEvent]]

final case class Stateful[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw : StatefulDraw, 
  Place[+_] : FlatMap,
  Recomposition,
  RaiseableEvent,
  HandleableEvent,
  ChildRaiseableEvent
](
    name: String,
    state: State[[W] =>> Update[W, RaiseableEvent], ChildRaiseableEvent, Place[Widget[Update, Draw, Place, Recomposition, ChildRaiseableEvent, HandleableEvent]]],
    childTree: Widget[Update, Draw, Place, Recomposition, ChildRaiseableEvent, HandleableEvent],
    runRecomposition : RecompositionRunner[Update, Draw, Place, Recomposition, HandleableEvent]
)  extends Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]:
  private type WidgetTree[+A] = Widget[Update, Draw, Place, Recomposition, A, HandleableEvent]
  private type FreeWidgetTree[+A] = Place[WidgetTree[A]]
  private type StatefulUpdateResult = Update[FreeWidgetTree[RaiseableEvent], RaiseableEvent]
  private type InternalState = State[[A] =>> Update[A, RaiseableEvent], ChildRaiseableEvent, FreeWidgetTree[ChildRaiseableEvent]]

  private def freeStateful(
                            state: InternalState,
                            childTree: Place[Widget[Update, Draw, Place, Recomposition, ChildRaiseableEvent, HandleableEvent]]
                          ) : FreeWidgetTree[RaiseableEvent] =
    childTree.map(Stateful(name, state, _, runRecomposition))
  end freeStateful

  override def draw: Draw = 
    summon[StatefulDraw[Draw]].drawStateful(this.name, this.state, this.childTree)
  end draw
  
  override def handleDownEvent(pathToParent: Path, event: HandleableEvent): StatefulUpdateResult =
    val pathToSelf = pathToParent.appendLast(name)
    onChildUpdate(pathToSelf, this.childTree.handleDownEvent(pathToSelf, event))
  end handleDownEvent

  override def asFree: FreeWidgetTree[RaiseableEvent] =
    freeStateful(this.state, this.childTree.asFree)
  end asFree

  override def mergeWithState(pathToParent: Path, oldState: Map[String, Any]): FreeWidgetTree[RaiseableEvent] =
    val pathToSelf = pathToParent.appendLast(name)
    oldState.get(this.name) match
      case Some(value) =>
        val newState = this.state.mergeWithOldState(value)
        val mergedChildTree = mergeFreeTrees(pathToSelf, this.childTree.asFree, newState.render)
        freeStateful(newState, mergedChildTree)  
      case None =>
        asFree
    end match
  end mergeWithState

  override def childrenStates: Map[String, Any] = 
    Map(this.name -> this.state.state)
  end childrenStates

  private def onChildUpdate(pathToSelf: Path, newChildF: Update[FreeWidgetTree[ChildRaiseableEvent], ChildRaiseableEvent]) : StatefulUpdateResult =
    for
      (newChild, events) <- newChildF.catchEvents
      (newState, newTree) <- events.foldLeftM[[T] =>> Update[T, RaiseableEvent], (InternalState, FreeWidgetTree[ChildRaiseableEvent])]((this.state, mergeFreeTrees(pathToSelf, this.childTree.asFree, newChild)))(
        (stateAndTree, event) =>
          stateAndTree._1.handleEvent(event).map(
            newState => (newState, mergeFreeTrees(pathToSelf, stateAndTree._2, newState.render))
          )
      )
    yield freeStateful(newState, newTree)
  end onChildUpdate

  override def aliveWidgets(currentPath: Path): Set[Path] =
    val pathToSelf = currentPath.appendFirst(name)
    Set(pathToSelf) ++ childTree.aliveWidgets(pathToSelf)
  end aliveWidgets
  
  private def mergeFreeTrees[A](pathToSelf: Path, oldOne: Place[WidgetTree[A]], newOne: Place[WidgetTree[A]]) : Place[WidgetTree[A]] =
    for 
      oldPlaced <- oldOne
      newPlaced <- newOne
      newMerged <- newPlaced.mergeWithState(pathToSelf, oldPlaced.childrenStates)
      recomposed <- runRecomposition(oldPlaced, newMerged)
    yield recomposed
  end mergeFreeTrees

  override def recomposed(currentPath : Path): Recomposition =
    childTree.recomposed(currentPath.appendFirst(name))
  end recomposed
end Stateful
