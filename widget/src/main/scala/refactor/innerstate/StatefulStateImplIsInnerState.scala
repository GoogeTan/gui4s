package me.katze.gui4s.widget
package refactor.innerstate

import refactor.StatefulState

import cats.Functor
import cats.syntax.all.*

final class StatefulStateImplIsInnerState[
  Merge[_] : Functor,
  State : Equiv as EQ,
  TaskSupervisor,
  Draw,
  EventHandler,
  RecompositionReaction
](
  typeCheckState : Any => Merge[(State, State)]
) extends InnerStates[
  StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction],
  Merge[StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction]],
  RecompositionReaction
]:
  override def innerStates(
                            self: StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction]
                          ): Map[String, StateTree[RecompositionReaction]] =
    Map(
      self.name -> StateTree((self.initialState, self.currentState), self.destructor(self.currentState), Map())
    )
  end innerStates

  override def mergeWithOldInnerStates(
                                        self: StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction],
                                        innerStates: Map[String, StateTree[RecompositionReaction]]
                                      ): Merge[StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction]] =
    typeCheckState(
      innerStates(self.name)
    ).map((oldInitialState, oldState) =>
      if EQ.equiv(oldInitialState, self.initialState) then
        self.copy(currentState = oldState) // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
      else
        self
    )
  end mergeWithOldInnerStates
end StatefulStateImplIsInnerState
