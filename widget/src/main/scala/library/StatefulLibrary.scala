package me.katze.gui4s.widget
package library

import library.lowlevel.WidgetLibrary
import me.katze.gui4s.widget.stateful.*

import cats.*
import cats.syntax.all.{*, given}

import scala.runtime.stdLibPatches.Predef.summon

def stateful[T: Equiv, ParentEvent, ChildEvent]
  (using lib: WidgetLibrary {
    type WidgetTask[+A]
    type Update[+U, +E] = EventReaction[WidgetTask[Any], U, E]
  })
  (using statefulFabric : FreeStatefulFabric[
          [U] =>> lib.Update[U, ParentEvent],
          lib.Merge,
          lib.WidgetTask,
          lib.FreeWidget,
          ParentEvent, lib.SystemEvent,
          ChildEvent, lib.SystemEvent
  ]
)(
  name: String,
  initialState: T,
  eventHandler: (T, ChildEvent) => EventReaction[lib.WidgetTask[ChildEvent], T, ParentEvent],
  renderState: T => lib.Widget[ChildEvent]
)(
  using RichTypeChecker[ChildEvent], RichTypeChecker[(T, T)], BiMonad[lib.Update], Monad[lib.Merge]
): lib.Widget[ParentEvent] =
  final case class StateImpl(
                              initialState: T,
                              currentState: T
                            ) extends State[[U] =>> lib.Update[U, ParentEvent], lib.Merge, lib.WidgetTask[ChildEvent], ChildEvent, ParentEvent, lib.Widget[ChildEvent]]:
    override def handleEvent(event: ChildEvent): lib.Update[State[[U] =>> lib.Update[U, ParentEvent], lib.Merge, lib.WidgetTask[ChildEvent], ChildEvent, ParentEvent, lib.Widget[ChildEvent]], ParentEvent] =
      eventHandler(currentState, event).mapState(StateImpl(initialState, _))
    end handleEvent

    override def render: lib.Widget[ChildEvent] = renderState(currentState)

    override def state: Any = (initialState, currentState)

    override def mergeWithOldState(maybeOldState: Any): lib.Merge[State[[U] =>> lib.Update[U, ParentEvent], lib.Merge, lib.WidgetTask[ChildEvent], ChildEvent, ParentEvent, lib.Widget[ChildEvent]]] =
      val (oldInitialState, oldState) = summon[RichTypeChecker[(T, T)]].tryCast(maybeOldState).valueOr(error => throw Exception(error))
      if Equiv[T].equiv(oldInitialState, initialState) then
        StateImpl(oldInitialState, currentState).pure[lib.Merge]
      else
        this.pure[lib.Merge]
      end if
    end mergeWithOldState
  end StateImpl

  val state = StateImpl(initialState, initialState)
  statefulFabric(name, state, state.render)
end stateful
