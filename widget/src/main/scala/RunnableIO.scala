package me.katze.gui4s.widget

import stateful.Path

final case class RunnableIO[+WidgetTask](io: WidgetTask, path: Path, keepAliveAfterWidgetDetach: Boolean)