package me.katze.gui4s.widget


final case class EventReaction[+State, +ParentUpEvent, +WidgetTask](newState : State, parentEvent: List[ParentUpEvent], ios : List[WidgetTask])