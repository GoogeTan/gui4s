package me.katze.gui4s.widget
package stateful

import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

final case class Stateful[
  Draw : StatefulDraw, 
  WidgetTask[+_],
  FreeWidgetTree[+_, -_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Draw, WidgetTask[Any], FreeWidgetTree, RaisesEvent, HandlesEvent],
  RaiseableEvent,
  HandleableEvent >: TaskFinished,
  ChildRaiseableEvent,
  ChildHandleableEvent >: HandleableEvent,
](
  name: String,
  state: State[WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
  childTree: PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
)(
  using
    freeWidgetIsMergeable: Mergeable[FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
    freeStatefulFabric   : FreeStatefulFabric[WidgetTask, FreeWidgetTree, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent],
    ChildRaisableEventTypeChecker: RichTypeChecker[ChildRaiseableEvent]
)  extends PlacedWidget[Draw, WidgetTask[Any], FreeWidgetTree, RaiseableEvent, HandleableEvent]:
  import freeWidgetIsMergeable.*
  private type PlacedStatefulWidget = Stateful[Draw, WidgetTask, FreeWidgetTree, PlacedWidgetTree, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent]
  private type StatefulEventResult = EventResult[WidgetTask[Any], FreeWidgetTree[RaiseableEvent, HandleableEvent], RaiseableEvent]

  private def freeStateful(
                            state: State[WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
                            childTree: FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
                          ) : FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStatefulFabric(this.name, state, childTree)
  end freeStateful

  override def draw: Draw = summon[StatefulDraw[Draw]].drawStateful(this.name, this.state, this.childTree)
  
  override def handleDownEvent(event: HandleableEvent): StatefulEventResult =
    event match
      case task : TaskFinished => handleFinishedTask(task)
      case otherThing          => handleChildEvent(otherThing)
    end match
  end handleDownEvent

  override def asFree: FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    freeStateful(this.state, this.childTree.asFree)
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): FreeWidgetTree[RaiseableEvent, HandleableEvent] =
    oldState.get(this.name) match
      case Some(value) =>
        val newState = this.state.mergeWithOldState(value)
        freeStateful(newState, freeWidgetIsMergeable.merge(this.childTree.asFree, newState.render))
      case None => asFree
  end mergeWithState

  override def childrenStates: Map[String, Any] = Map(this.name -> this.state.state)

  private def handleFinishedTask(taskFinished: TaskFinished) : StatefulEventResult =
    taskFinished match
      case TaskFinished(this.name, Nil,                      maybeEvent   ) => handleFinishedTask(maybeEvent)
      case TaskFinished(this.name, childName :: furtherPath, eventForChild) => handleChildFinishedTask(childName, furtherPath, eventForChild)
      case _                                                                => handleSomeoneElsesTask()
    end match
  end handleFinishedTask

  private def handleSomeoneElsesTask(): StatefulEventResult =
    emptyEventResult()
  end handleSomeoneElsesTask

  private def handleFinishedTask(maybeEvent : Any): StatefulEventResult =
    val event = ChildRaisableEventTypeChecker.tryCast(maybeEvent).valueOr(castError => throw Exception(castError))
    handleEvent(event)
  end handleFinishedTask

  private def pathToSelf: Path = Path(List(this.name))

  private def handleChildFinishedTask(childWidgetName : String, furtherPath: List[String], eventForChild: Any): StatefulEventResult =
    handleEventResult(this.childTree.handleDownEvent(TaskFinished(childWidgetName, furtherPath, eventForChild)))
  end handleChildFinishedTask

  private def handleChildEvent(otherThing : HandleableEvent): StatefulEventResult =
    handleEventResult(this.childTree.handleDownEvent(otherThing))
  end handleChildEvent

  private def handleEventResult(eventResult: EventResult[WidgetTask[Any], FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent], ChildRaiseableEvent]) : StatefulEventResult =
    val EventResult(newChild, upEvent, tasks) = eventResult
    val fixedTasks = fixChildTasksPaths(tasks)
    upEvent.foldLeft(
      EventResult(
        freeStateful(this.state, merge(this.childTree.asFree, newChild)),
        Nil,
        fixedTasks
      )
    )(
      handleEvent
    )
  end handleEventResult

  def handleEvent(result : StatefulEventResult, event : ChildRaiseableEvent) : StatefulEventResult =
    val EventReaction(newState, parentEvent, tasks) = this.state.handleEvent(event)
    EventResult(
      freeStateful(
        newState,
        merge(this.childTree.asFree, newState.render)
      ),
      parentEvent.toList,
      result.ios ++ fixSelfTasks(tasks)
    )

  private def handleEvent(event : ChildRaiseableEvent) : StatefulEventResult =
    val EventReaction(newState, parentEvent, ios) = this.state.handleEvent(event)
    EventResult(
      freeStateful(newState, merge(this.childTree.asFree, newState.render)),
      parentEvent.toList,
      fixSelfTasks(ios)
    )
  end handleEvent

  private def emptyEventResult() : EventResult[WidgetTask[Nothing], FreeWidgetTree[RaiseableEvent, HandleableEvent], RaiseableEvent] =
    EventResult(widget = asFree)
  end emptyEventResult

  private def fixSelfTasks[T](tasks: List[(WidgetTask[T], Boolean)]): List[RunnableIO[WidgetTask[T]]] =
    tasks.map(RunnableIO(_, pathToSelf, _))
  end fixSelfTasks

  private def fixChildTasksPaths[T](tasks: List[RunnableIO[WidgetTask[T]]]) : List[RunnableIO[WidgetTask[T]]] =
    tasks.map(fixChildTaskPath)
  end fixChildTasksPaths

  private def fixChildTaskPath[T](task: RunnableIO[WidgetTask[T]]) : RunnableIO[WidgetTask[T]] =
    task.copy(path = task.path.appendFirst(this.name))
  end fixChildTaskPath

  override def filterDeadPaths(currentPath : Path, alive: Set[Path]): Set[Path] =
    val pathToSelf = currentPath.appendFirst(name)
    childTree.filterDeadPaths(pathToSelf, alive.excl(pathToSelf))
  end filterDeadPaths
end Stateful
