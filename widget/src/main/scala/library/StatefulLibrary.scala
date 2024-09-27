package me.katze.gui4s.widget
package library

import stateful.*

import cats.*
import cats.syntax.all.{*, given}

import scala.runtime.stdLibPatches.Predef.summon

trait StatefulLibrary extends WidgetLibrary:
  given statefulIsDrawable : StatefulDraw[Draw]
  
  def statefulFabric[
    RaiseableEvent, HandleableEvent >: TaskFinished,
    ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
  ](using RichTypeChecker[ChildRaiseableEvent]) : FreeStatefulFabric[
    WidgetTask,
    [A, B] =>> PlacementEffect[PlacedWidget[A, B]],
    RaiseableEvent, HandleableEvent,
    ChildRaiseableEvent, ChildHandleableEvent
  ]
  
  final def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                          name: String,
                                                          initialState: T,
                                                          eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ChildEvent, ParentEvent],
                                                       )(
                                                          renderState: T => Widget[ChildEvent]
                                                        )(
                                                          using RichTypeChecker[ChildEvent], RichTypeChecker[(T, T)]
                                                        ): Widget[ParentEvent] =
    case class StateImpl(initialState: T, currentState: T) extends State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]]:
      override def handleEvent(event: ChildEvent): EventReaction[WidgetTask[ChildEvent], State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]], ChildEvent, ParentEvent] =
        eventHandler(currentState, event).mapState(StateImpl(initialState, _))
      end handleEvent

      override def render: Widget[ChildEvent] = renderState(currentState)

      override def state: Any = (initialState, currentState)

      override def mergeWithOldState(maybeOldState: Any): State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]] =
        val (oldInitialState, oldState) = summon[RichTypeChecker[(T, T)]].tryCast(maybeOldState).valueOr(error => throw Exception(error))
        if Equiv[T].equiv(oldInitialState, initialState) then
          StateImpl(oldInitialState, currentState)
        else
          this
        end if
      end mergeWithOldState
    end StateImpl

    val state = StateImpl(initialState, initialState)
    statefulFabric(using summon[RichTypeChecker[ChildEvent]])(name, state, state.render)
  end stateful
end StatefulLibrary

