package gui4s.core.widget

final case class Stateful[
  PlacedWidget,
  StateBehaviour,
](
    name : String,
    stateBehaviour : StateBehaviour,
    child : PlacedWidget,
)
