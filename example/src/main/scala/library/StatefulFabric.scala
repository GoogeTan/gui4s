package me.katze.gui4s.example
package library

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.example.impl.FreeStatefulFabricImpl
import me.katze.gui4s.example.stateful.{EventReaction, Mergeable, RichTypeChecker, State, TaskFinished}

import scala.runtime.stdLibPatches.Predef.summon

trait StatefulFabric:
  type PlacedWidget[+A, -B] <: me.katze.gui4s.example.PlacedWidget[WidgetTask[Any], FreeT[PlacedWidget], A, B]
  type Widget[+A] = FreeT[PlacedWidget][A, SystemEvent]
  type WidgetTask[+_]
  type SystemEvent >: TaskFinished
  type PlacementEffect[+W]
  type FreeT[F[+_, -_]] = [A, B] =>> PlacementEffect[F[A, B]]

  given placementIsEffect: Monad[PlacementEffect]

  given freeTreesAreMergeable[Event]: Mergeable[Widget[Event]]

  def constructRealWidget[Event](widget: me.katze.gui4s.example.PlacedWidget[WidgetTask[Any], FreeT[PlacedWidget], Event, SystemEvent]): PlacedWidget[Event, SystemEvent]

  def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                    name: String,
                                                    initialState: T,
                                                    eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ChildEvent, ParentEvent],
                                                    renderChild: T => Widget[ChildEvent]
                                                  )(
                                                    using RichTypeChecker[ChildEvent], RichTypeChecker[(T, T)]
                                                  ): Widget[ParentEvent] =
    class StateImpl(initialState: T, currentState: T) extends State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]]:
      override def handleEvent(event: ChildEvent): EventReaction[WidgetTask[ChildEvent], State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]], ChildEvent, ParentEvent] =
        eventHandler(currentState, event).mapState(StateImpl(initialState, _))
      end handleEvent

      override def render: Widget[ChildEvent] = renderChild(currentState)

      override def state: Any = (initialState, currentState)

      override def prettyString: String = s"State(initial=${this.initialState}, current=${this.currentState})"

      override def wergeWithOldState(maybeOldState: Any): State[WidgetTask[ChildEvent], ChildEvent, ParentEvent, Widget[ChildEvent]] =
        val (oldInitialState, oldState) = summon[RichTypeChecker[(T, T)]].tryCast(maybeOldState).valueOr(error => throw Exception(error))
        if Equiv[T].equiv(oldInitialState, initialState) then
          StateImpl(oldInitialState, currentState)
        else
          this
        end if
      end wergeWithOldState
    end StateImpl

    val statefulFabric = FreeStatefulFabricImpl[WidgetTask, PlacedWidget, PlacementEffect, ParentEvent, SystemEvent, ChildEvent, SystemEvent](constructRealWidget)
    val state = StateImpl(initialState, initialState)
    statefulFabric(name, state, state.render)
  end stateful
end StatefulFabric

