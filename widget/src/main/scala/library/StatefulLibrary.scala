package me.katze.gui4s.widget
package library

import stateful.{*, given}

import cats.*
import cats.syntax.all.*

def statefulWidget[
  Update[+_, +_] : {BiMonad, CatchEvents, LiftEventReaction},
  Draw : StatefulDraw,
  Place[+_] : FlatMap,
  Recomposition,
  State: {Equiv, RichTypeChecker},
  ParentEvent, ChildEvent,
  DownEvent,
  WidgetTask[+_]
](
   name          : String,
   initialState  : State,
   deallocState : State => Recomposition,
   eventHandler  : (State, ChildEvent) => EventReaction[State, ParentEvent, WidgetTask[ChildEvent]],
   renderState   : State => Place[Widget[[Value] =>> Update[Value, ChildEvent], Draw, Place, Recomposition, DownEvent]],
   runTasks : List[WidgetTask[ChildEvent]] => Update[Unit, Nothing]
): Place[Widget[[Value] =>> Update[Value, ParentEvent], Draw, Place, Recomposition, DownEvent]] =
  statefulWidget_(
    name,
    InitialBasedState(initialState, initialState, eventHandler, renderState, summon, deallocState),
    runTasks,
  )
end statefulWidget

def statefulWidget_[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Draw : StatefulDraw,
  Place[+_] : FlatMap,
  Recomposition,
  ParentEvent, ChildEvent,
  DownEvent,
  Task,
](
   name          : String,
   state : State[[Value] =>> Update[Value, ParentEvent], Recomposition, ChildEvent, Place[Widget[[Value] =>> Update[Value, ChildEvent], Draw, Place, Recomposition, DownEvent]], Task],
   runTasks : List[Task] => Update[Unit, Nothing]
): Place[Widget[[Value] =>> Update[Value, ParentEvent], Draw, Place, Recomposition, DownEvent]] =
  state.render.map(Stateful(name, state, _, runTasks))
end statefulWidget_
