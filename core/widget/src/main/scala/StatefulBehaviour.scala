package gui4s.core.widget

import cats.syntax.all.*
import scala.reflect.Typeable

final case class StatefulState[State](initialState : State, currentState : State):
  override def toString: String =
    "StatefulState(initialState = " + initialState + ", currentState = " + currentState + ")"
  end toString
  
  def withNewState(state : State) : StatefulState[State] =
    copy(currentState = state)
  end withNewState
end StatefulState

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given[State : Typeable] : Typeable[StatefulState[State]] = (a : Any) => a match
  case StatefulState(_: State, _: State) =>
    a.asInstanceOf[a.type & StatefulState[State]].some
  case _ =>
    None
end given

final case class StatefulBehaviour[
  State,
  Draw,
  EventHandler,
  Destructor
](
    name : String,
    state: StatefulState[State],
    draw : Draw,
    handleEvents : EventHandler,
    destructor: Destructor,
):
  def withNewState(state: State) : StatefulBehaviour[State, Draw, EventHandler, Destructor] =
    copy(state = this.state.withNewState(state))
  end withNewState
  
  override def toString: String =
    "StatefulState(name = \"" + name + "\", state={" + state.toString + "})"
  end toString

  override def equals(obj: Any): Boolean =
    obj match
      case that: StatefulBehaviour[_, _, _, _] =>
        this.name == that.name && this.state == that.state
      case _ => false
    end match
  end equals
end StatefulBehaviour

given[State : Equiv as sEq] : Equiv[StatefulState[State]] =
  (a, b) =>
    sEq.equiv(a.initialState, b.initialState) && sEq.equiv(a.currentState, b.currentState)
end given

given[State : Equiv as sEq, Draw, EventHandler, Destructor] : Equiv[StatefulBehaviour[State, Draw, EventHandler, Destructor]] =
  (a, b) =>
    a.name == b.name && Equiv[StatefulState[State]].equiv(a.state, b.state)
end given
