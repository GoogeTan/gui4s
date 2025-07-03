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

