package gui4s.core.widget

import cats.Eq
import cats.syntax.all.*
import gui4s.core.widget.StatefulState.statefulStateEqInstance

final case class StatefulBehaviour[
  State,
  Draw,
  EventHandler
](
  name : String,
  state: StatefulState[State],
  draw : Draw,
  handleEvents : EventHandler,
):
  def withNewState(state: State) : StatefulBehaviour[State, Draw, EventHandler] =
    copy(state = this.state.withNewState(state))
  end withNewState

  override def toString: String =
    "StatefulState(name = \"" + name + "\", state={" + state.toString + "})"
  end toString
end StatefulBehaviour

object StatefulBehaviour:
  given statefulBehaviourEqInstance[State : Eq as sEq, Draw, EventHandler] : Eq[StatefulBehaviour[State, Draw, EventHandler]] =
    (a, b) =>
      a.name === b.name && statefulStateEqInstance.eqv(a.state, b.state)
  end statefulBehaviourEqInstance
end StatefulBehaviour
