package me.katze.gui4s.widget
package stateful

import cats.FlatMap
import cats.syntax.all.*
import me.katze.gui4s.widget

final case class Stateful[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Draw : StatefulDraw as SD, 
  Place[+_] : FlatMap,
  Task,
  Recomposition,
  RaiseableEvent,
  HandleableEvent,
  ChildRaiseableEvent
](
    name: String,
    state: State[[W] =>> Update[W, RaiseableEvent], Recomposition, ChildRaiseableEvent, Place[Widget[[E] =>> Update[E, ChildRaiseableEvent], Draw, Place, Recomposition, HandleableEvent]], Task],
    childTree: Widget[[E] =>> Update[E, ChildRaiseableEvent], Draw, Place, Recomposition, HandleableEvent],
    runTasks : List[Task] => Update[Unit, Nothing]
)  extends Widget[[E] =>> Update[E, RaiseableEvent], Draw, Place, Recomposition, HandleableEvent]:
  private type WidgetTree[+A] = Widget[[E] =>> Update[E, A], Draw, Place, Recomposition, HandleableEvent]
  private type FreeWidgetTree[+A] = Place[WidgetTree[A]]
  private type StatefulUpdateResult = Update[FreeWidgetTree[RaiseableEvent], RaiseableEvent]
  private type InternalState = State[[A] =>> Update[A, RaiseableEvent], Recomposition, ChildRaiseableEvent, FreeWidgetTree[ChildRaiseableEvent], Task]

  private def freeStateful(
                            state: InternalState,
                            childTree: Place[Widget[[E] =>> Update[E, ChildRaiseableEvent], Draw, Place, Recomposition, HandleableEvent]]
                          ) : FreeWidgetTree[RaiseableEvent] =
    childTree.map(Stateful(name, state, _, runTasks))
  end freeStateful

  override def draw: Draw = 
    SD.drawStateful(this.name, this.state, this.childTree)
  end draw
  
  override def handleDownEvent(pathToParent: Path, event: HandleableEvent): StatefulUpdateResult =
    val pathToSelf = pathToParent.appendLast(name)
    onChildUpdate(pathToSelf, this.childTree.handleDownEvent(pathToSelf, event))
  end handleDownEvent

  override def asUnplaced: FreeWidgetTree[RaiseableEvent] =
    freeStateful(this.state, this.childTree.asUnplaced)
  end asUnplaced

  override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): FreeWidgetTree[RaiseableEvent] =
    val pathToSelf = pathToParent.appendLast(name)
    oldState.get(this.name) match
      case Some(prevStateTree) =>
        val newState = this.state.mergeWithOldState(prevStateTree.state)
        val mergedChildTree = mergeFreeTrees(pathToSelf, this.childTree.asUnplaced, newState.render)
        freeStateful(newState, mergedChildTree)  
      case None =>
        asUnplaced
    end match
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    val stateValue = state
    Map(this.name -> StateTree[Recomposition](stateValue.state, stateValue.dealloc, childTree.childrenStates))
  end childrenStates

  private def onChildUpdate(pathToSelf: Path, newChildF: Update[FreeWidgetTree[ChildRaiseableEvent], ChildRaiseableEvent]) : StatefulUpdateResult =
    for
      (newChild, events) <- newChildF.catchEvents
      (newState, newTree) <- events.foldLeftM[[T] =>> Update[T, RaiseableEvent], (InternalState, FreeWidgetTree[ChildRaiseableEvent])]((this.state, mergeFreeTrees(pathToSelf, this.childTree.asUnplaced, newChild)))(
        (stateAndTree, event) =>
          val (newState, tasks) = stateAndTree._1.handleEvent(event) // TODO run tasks
          runTasks(tasks) *> newState.map(newState => (newState, mergeFreeTrees(pathToSelf, stateAndTree._2, newState.render)))
      )
    yield freeStateful(newState, newTree)
  end onChildUpdate

  private def mergeFreeTrees[A](pathToSelf: Path, oldOne: Place[WidgetTree[A]], newOne: Place[WidgetTree[A]]) : Place[WidgetTree[A]] =
    FlatMap[Place].flatMap2(oldOne, newOne)((newPlaced, oldPlaced) => newPlaced.mergeWithState(pathToSelf, oldPlaced.childrenStates))
  end mergeFreeTrees

  override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition =
    states.get(this.name) match
      case Some(prevState) =>
        childTree.recomposed(currentPath.appendFirst(name), prevState.childrenStates)
      case None =>
        childTree.recomposed(currentPath.appendFirst(name), Map())
    end match
  end recomposed
end Stateful
