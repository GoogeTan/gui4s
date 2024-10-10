package me.katze.gui4s.widget

final case class EventResult[+WidgetTask, +FreeWidget, +UpEvent](
                                                                  widget: FreeWidget, 
                                                                  upEvent: List[UpEvent] = Nil,
                                                                  ios   : List[RunnableIO[WidgetTask]] = Nil
                                                                )
