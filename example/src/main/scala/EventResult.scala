package me.katze.gui4s.example

import stateful.Path

final case class EventResult[+WidgetTask, +FreeWidget, +UpEvent](
                                                              widget: FreeWidget, 
                                                              upEvent: Option[UpEvent] = None,
                                                              ios   : List[(Path, WidgetTask)] = Nil
                                                            )