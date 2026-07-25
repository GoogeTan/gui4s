package gui4s.core.widget

import cats.Eq
import scala.reflect.Typeable
import cats.syntax.all.*

final case class StatefulState[State](initialState : State, currentState : State):
  override def toString: String =
    "StatefulState(initialState = " + initialState + ", currentState = " + currentState + ")"
  end toString

  def withNewState(state : State) : StatefulState[State] =
    copy(currentState = state)
  end withNewState
end StatefulState

object StatefulState:
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  given[State : Typeable] : Typeable[StatefulState[State]] = (a : Any) => a match
    case StatefulState(_: State, _: State) =>
      a.asInstanceOf[a.type & StatefulState[State]].some
    case _ =>
      None
  end given

  given statefulStateEqInstance[State: Eq as sEq]: Eq[StatefulState[State]] =
    (a, b) =>
      sEq.eqv(a.initialState, b.initialState) && sEq.eqv(a.currentState, b.currentState)
  end statefulStateEqInstance
end StatefulState

