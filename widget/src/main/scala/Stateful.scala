package me.katze.gui4s.widget

final case class Stateful[
  PlacedWidget,
  State,
](
  name : String,
  state : State,
  child : PlacedWidget,
)
