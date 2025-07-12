package me.katze.gui4s.widget

final case class StatefulState[
  State,
  Draw,
  EventHandler,
  Destructor
](
    name : String,
    initialState : State,
    currentState : State,
    draw : Draw,
    handleEvents : EventHandler,
    destructor : Destructor
):
  override def toString: String =
    "StatefulState(name = \"" + name + "\", initialState={" + initialState.toString + "}, currentState={" + currentState.toString + "})"

given[State : Equiv as sEq, Draw, EventHandler, Destructor] : Equiv[StatefulState[State, Draw, EventHandler, Destructor]] = 
  (a, b) =>
    sEq.equiv(a.initialState, b.initialState) && sEq.equiv(a.currentState, b.currentState)
end given
