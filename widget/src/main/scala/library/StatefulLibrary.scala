package me.katze.gui4s.widget
package library

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

def stateful[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw : StatefulDraw,
  Place[+_] : FlatMap,
  Recomposition,
  T: Equiv,
  ParentEvent, ChildEvent,
  DownEvent,
  WidgetTask[+_]
](
    using liftEventReaction : LiftEventReaction[Update, WidgetTask[Any]]
)(
    name          : String,
    initialState  : T,
    eventHandler  : (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent],
    renderState   : T => Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]],
    typeCheck     : RichTypeChecker[(T, T)],
    runRecomposition : RecompositionRunner[Update, Draw, Place, Recomposition, DownEvent]
): Place[Widget[Update, Draw, Place, Recomposition, ParentEvent, DownEvent]] =
  final case class StateImpl(
                              initialState: T,
                              currentState: T
                            ) extends State[[U] =>> Update[U, ParentEvent], ChildEvent, Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]]]:
    override def handleEvent(event: ChildEvent): Update[State[[U] =>> Update[U, ParentEvent], ChildEvent, Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]]], ParentEvent] =
      liftEventReaction.lift(
        eventHandler(currentState, event).mapState(StateImpl(initialState, _))
      )
    end handleEvent

    override def render: Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]] = renderState(currentState)

    override def state: Any = (initialState, currentState)

    override def mergeWithOldState(maybeOldState: Any): State[[U] =>> Update[U, ParentEvent], ChildEvent, Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]]] =
      val (oldInitialState, oldState) = typeCheck.tryCast(maybeOldState).valueOr(error => throw Exception(error))
      if Equiv[T].equiv(oldInitialState, initialState) then
        StateImpl(oldInitialState, currentState)
      else
        this
      end if
    end mergeWithOldState
  end StateImpl

  val state = StateImpl(initialState, initialState)
  state.render.map(Stateful(name, state, _, runRecomposition))
end stateful
