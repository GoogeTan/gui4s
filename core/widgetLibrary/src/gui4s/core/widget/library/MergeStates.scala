package gui4s.core.widget.library

import cats.Eq
import gui4s.core.widget.StatefulState
import scala.reflect.Typeable

//TODO document me
type MergeStates[Place[_], State] = [T] => (Any, StatefulState[State], StatefulState[State] => Place[T]) => Option[Place[T]]

//TODO может ещё сравнивать текущие состояния, чтобы уменьшить чисто рекомпозиций?
object MergeStates:
  def withConstInitialState[Place[_], State: Typeable](onMistypeError : [T] => Any => Place[T]): MergeStates[Place, State] =
    [T] => (oldStateAny: Any, currentState, callback) =>
      (oldStateAny, currentState) match
        case (StatefulState(_, state: State), StatefulState(initialState, _)) =>
          Some(callback(StatefulState(initialState, state)))
        case _ =>
          Some(onMistypeError(oldStateAny))
  end withConstInitialState

  def default[
    Place[_],
    State : {Eq as EQ, Typeable}
  ](onMistypeError : [T] => Any => Place[T]) : MergeStates[Place, State] =
    [T] => (oldState, newState, consumer) =>
      oldState match
        case StatefulState(initialState : State, currentState : State) =>
          if EQ.eqv(initialState, newState.initialState) then
            Some(consumer(StatefulState(initialState, currentState))) // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
          else
            None
        case _ =>
          Some(onMistypeError(oldState))
      end match
  end default
end MergeStates