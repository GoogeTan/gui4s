package me.katze.gui4s.widget

import stateful.Path

final case class RunnableIO[+WidgetTask](io: WidgetTask, path: Path, keepAliveAfterWidgetDetach: Boolean)

final case class EventResult[+WidgetTask, +FreeWidget, +UpEvent](
                                                                  widget: FreeWidget, 
                                                                  upEvent: List[UpEvent] = Nil,
                                                                  ios   : List[RunnableIO[WidgetTask]] = Nil
                                                                )