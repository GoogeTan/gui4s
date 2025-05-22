package me.katze.gui4s.widget
package refactor

final case class StatefulState[
  State,
  TaskSupervisor,
  Draw,
  EventHandler,
  Destructor
](
    name : String,
    initialState : State,
    currentState : State,
    taskSupervisor: TaskSupervisor,
    draw : Draw,
    handleEvents : EventHandler,
    destructor : Destructor
)
