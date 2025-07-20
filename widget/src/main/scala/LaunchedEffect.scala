package me.katze.gui4s.widget

final case class LaunchedEffect[
  Key,
  Task,
](
  name : String,
  key : Key,
  taskOnChange : Task,
)
