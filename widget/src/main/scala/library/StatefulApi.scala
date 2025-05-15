package me.katze.gui4s.widget
package library

import stateful.{*, given}

import catnip.BiMonad
import catnip.syntax.bimonad.given
import cats.syntax.all.*
import cats.{FlatMap, Functor, Monoid}
import me.katze.gui4s.widget.Widget
import me.katze.gui4s.widget.statefulWidget as rawStatefulWidget

type StatefulWidget[Widget[_], WidgetTask[_], Recomposition, TaskSupervisor] =
  [
    State: {Equiv, RichTypeChecker},
    ParentEvent,
    ChildEvent: RichTypeChecker
  ] => (
    name: String,
    initialState: State,
    dealloc: State => Recomposition,
    eventHandler: (State, ChildEvent) => EventReaction[State, ParentEvent, WidgetTask[ChildEvent]],
    supervisor: TaskSupervisor
  ) => (
    renderState: State => Widget[ChildEvent]
  ) => Widget[ParentEvent]

type StateInBetweenWidget[Widget[_], WidgetTask[_], Recomposition, TaskSupervisor] =
  [
    State: {Equiv, RichTypeChecker},
    TransitiveEvent: RichTypeChecker,
    OwnEvent: RichTypeChecker
  ] => (
    name: String,
    initialState: State,
    dealloc: State => Recomposition,
    eventHandler: (State, OwnEvent) => EventReaction[State, TransitiveEvent, WidgetTask[OwnEvent]],
    supervisor: TaskSupervisor
  ) => (
    renderState: State => Widget[Either[TransitiveEvent, OwnEvent]]
  ) => Widget[TransitiveEvent]


def statefulWidget[
  Update[+_, +_]: {BiMonad, CatchEvents, LiftEventReaction},
  Draw : StatefulDraw,
  Place[+_] : FlatMap,
  Recomposition,
  WidgetTask[+_] : Functor,
  SystemEvent >: TaskFinished,
  Supervisor,
](
   raiseEvent : [Event] => () => RaiseEvent[Update[Unit, Event]],
   runOnSupervisor : (Supervisor, List[WidgetTask[Any]]) => Update[Unit, Nothing]
) : StatefulWidget[[Event] =>> Place[Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]], WidgetTask, Recomposition, Supervisor] =
  def addTaskResultCatcher[Event: RichTypeChecker](name: String)(initial: Place[Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]]): Place[Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]] =
    given RaiseEvent[Update[Unit, Event]] = raiseEvent[Event]()
    Functor[Place].map(initial)(TaskResultCatcher(name, _))
  end addTaskResultCatcher

  [
    State: {Equiv, RichTypeChecker},
    ParentEvent,
    ChildEvent: RichTypeChecker
  ] => (
    name: String,
    initialState: State,
    dealloc_ : State => Recomposition,
    eventHandler: (State, ChildEvent) => EventReaction[State, ParentEvent, WidgetTask[ChildEvent]],
    supervisor: Supervisor
  ) => (renderState: State => Place[Widget[[Value] =>> Update[Value, ChildEvent], Draw, Place, Recomposition, SystemEvent]]) =>
    val render = renderState andThen addTaskResultCatcher[ChildEvent](name)
    rawStatefulWidget[Update, Draw, Place, Recomposition, State, ParentEvent, ChildEvent, SystemEvent, WidgetTask](name, initialState, dealloc_, eventHandler, render, runOnSupervisor(supervisor, _))
end statefulWidget


// TODO Надо подумать, может такое состояние должно стоять не над, а рядом с детскими состояниями, чтобы его можно было добавлять и убавлять.
def stateInBetweenWidget[
  Update[+_, +_] : {BiMonad, CatchEvents, LiftEventReaction},
  Draw: {Monoid, StatefulDraw },
  Place[+_] : FlatMap,
  Recomposition,
  WidgetTask[+_] : Functor,
  SystemEvent >: TaskFinished,
  Supervisor,
](
    raiseEvent: [Event] => () => RaiseEvent[Update[Unit, Event]],
    runOnSupervisor: (Supervisor, List[WidgetTask[Any]]) => Update[Unit, Nothing]
  ): StateInBetweenWidget[[Event] =>> Place[Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]], WidgetTask, Recomposition, Supervisor] =
  [
    State: {Equiv, RichTypeChecker},
    TransitiveEvent: RichTypeChecker,
    OwnEvent: RichTypeChecker
  ] => (
      name: String,
      initialState: State,
      dealloc: State => Recomposition,
      eventHandler: (State, OwnEvent) => EventReaction[State, TransitiveEvent, WidgetTask[OwnEvent]], 
      supervisor: Supervisor
  ) => (renderState: State => Place[Widget[[W] =>> Update[W, Either[TransitiveEvent, OwnEvent]], Draw, Place, Recomposition, SystemEvent]]) =>
      statefulWidget(raiseEvent, runOnSupervisor)[State, TransitiveEvent, Either[TransitiveEvent, OwnEvent]](
      name,
      initialState,
      dealloc,
      (state, event) =>
        event match
          case Left(transitiveEvent) => EventReaction(state, List(transitiveEvent), Nil)
          case Right(ownEvent) => eventHandler(state, ownEvent).mapIOS(_.map(Right(_))),
      supervisor
    )(renderState)
end stateInBetweenWidget
