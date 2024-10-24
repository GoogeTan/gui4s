package me.katze.gui4s.widget
package library

import library.lowlevel.WidgetLibrary
import me.katze.gui4s.widget.stateful.*

import cats.*
import cats.syntax.all.{*, given}

import scala.runtime.stdLibPatches.Predef.summon

trait LiftEventReaction[
  Update[+_, +_],
  WidgetTask,
]:
  def lift[A, B](reaction : EventReaction[WidgetTask, A, B]) : Update[A, B]
end LiftEventReaction

def stateful[T: Equiv, ParentEvent, ChildEvent, WidgetTask[+_]]
  (using lib: WidgetLibrary)
  (using liftEventReaction : LiftEventReaction[lib.Update, WidgetTask[Any]]
)(
    statefulFabric : FreeStatefulFabric[
    [U] =>> lib.Update[U, ParentEvent],
    lib.FreeWidget,
    ParentEvent, lib.SystemEvent,
    ChildEvent, lib.SystemEvent
  ],
    name          : String,
    initialState  : T,
    eventHandler  : (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent],
    renderState   : T => lib.Widget[ChildEvent]
)(
  using RichTypeChecker[(T, T)]
): lib.Widget[ParentEvent] =
  final case class StateImpl(
                              initialState: T,
                              currentState: T
                            ) extends State[[U] =>> lib.Update[U, ParentEvent], ChildEvent, lib.Widget[ChildEvent]]:
    override def handleEvent(event: ChildEvent): lib.Update[State[[U] =>> lib.Update[U, ParentEvent], ChildEvent, lib.Widget[ChildEvent]], ParentEvent] =
      liftEventReaction.lift(
        eventHandler(currentState, event).mapState(StateImpl(initialState, _))
      )
    end handleEvent

    override def render: lib.Widget[ChildEvent] = renderState(currentState)

    override def state: Any = (initialState, currentState)

    override def mergeWithOldState(maybeOldState: Any): State[[U] =>> lib.Update[U, ParentEvent], ChildEvent, lib.Widget[ChildEvent]] =
      val (oldInitialState, oldState) = summon[RichTypeChecker[(T, T)]].tryCast(maybeOldState).valueOr(error => throw Exception(error))
      if Equiv[T].equiv(oldInitialState, initialState) then
        StateImpl(oldInitialState, currentState)
      else
        this
      end if
    end mergeWithOldState
  end StateImpl

  val state = StateImpl(initialState, initialState)
  statefulFabric(name, state, state.render)
end stateful
