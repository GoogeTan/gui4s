package gui4s.core.widget

import cats.syntax.all.*

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
  given[State : Equiv as sEq, Draw, EventHandler] : Equiv[StatefulBehaviour[State, Draw, EventHandler]] =
    (a, b) =>
      a.name === b.name && Equiv[StatefulState[State]].equiv(a.state, b.state)
  end given
end StatefulBehaviour
