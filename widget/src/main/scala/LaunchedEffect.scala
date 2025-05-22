package me.katze.gui4s.widget

final case class LaunchedEffect[
  Key,
  Recomposition
](
  name : String,
  key : Key,
  taskOnChange : Path => Recomposition
)
