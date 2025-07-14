package me.katze.gui4s.widget

final case class Stateful[
  PlacedWidget,
  StateBehaviour,
](
    name : String,
    stateBehaviour : StateBehaviour,
    child : PlacedWidget,
)
