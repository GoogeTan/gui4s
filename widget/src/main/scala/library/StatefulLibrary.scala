package me.katze.gui4s.widget
package library

import stateful.*

import cats.*
import cats.syntax.all.{*, given}

def statefulWidget[
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
    deallocState : T => Recomposition,
    eventHandler  : (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent],
    renderState   : T => Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]],
    typeCheck     : RichTypeChecker[(T, T)]
): Place[Widget[Update, Draw, Place, Recomposition, ParentEvent, DownEvent]] =
  statefulWidget(
    name,
    InitialBasedState(initialState, initialState, eventHandler, renderState, typeCheck, deallocState, liftEventReaction)
  )
end statefulWidget

def statefulWidget[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw : StatefulDraw,
  Place[+_] : FlatMap,
  Recomposition,
  ParentEvent, ChildEvent,
  DownEvent,
](
    name          : String,
    state : State[[U] =>> Update[U, ParentEvent], Recomposition, ChildEvent, Place[Widget[Update, Draw, Place, Recomposition, ChildEvent, DownEvent]]]
): Place[Widget[Update, Draw, Place, Recomposition, ParentEvent, DownEvent]] =
  state.render.map(Stateful(name, state, _))
end statefulWidget
