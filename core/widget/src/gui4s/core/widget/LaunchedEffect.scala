package gui4s.core.widget

final case class LaunchedEffect[
  Key,
  Task,
](
  name : String,
  key : Key,
  taskOnChange : Task,
)
