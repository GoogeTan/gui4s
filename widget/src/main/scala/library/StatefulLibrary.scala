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
  T: {Equiv, RichTypeChecker},
  ParentEvent, ChildEvent,
  DownEvent,
  WidgetTask[+_]
](
    name          : String,
    initialState  : T,
    deallocState : T => Recomposition,
    eventHandler  : (T, ChildEvent) => EventReaction[T, ParentEvent, WidgetTask[ChildEvent]],
    renderState   : T => Place[Widget[[W] =>> Update[W, ChildEvent], Draw, Place, Recomposition, DownEvent]],
    runTasks : List[WidgetTask[ChildEvent]] => Update[Unit, Nothing]
): Place[Widget[[W] =>> Update[W, ParentEvent], Draw, Place, Recomposition, DownEvent]] =
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
    state : State[[U] =>> Update[U, ParentEvent], Recomposition, ChildEvent, Place[Widget[[W] =>> Update[W, ChildEvent], Draw, Place, Recomposition, DownEvent]], Task],
    runTasks : List[Task] => Update[Unit, Nothing]
): Place[Widget[[W] =>> Update[W, ParentEvent], Draw, Place, Recomposition, DownEvent]] =
  state.render.map(Stateful(name, state, _, runTasks))
end statefulWidget_
