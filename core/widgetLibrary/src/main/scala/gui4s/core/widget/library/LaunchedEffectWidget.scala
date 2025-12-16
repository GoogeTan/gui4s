package gui4s.core.widget.library

type LaunchedEffectWidget[Widget, Key, Task] = (name : String, child : Widget, key : Key, task : Task) => Widget
