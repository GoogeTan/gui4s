package me.katze.gui4s.widget
package stateful

final case class EventReaction[+WidgetTask, +State, +UpEvent, +ParentUpEvent](
                                                                              newState   : State,
                                                                              parentEvent: Option[ParentUpEvent] = None,
                                                                              ios        : List[(WidgetTask, Boolean)] = Nil
                                                                            ):
  def mapState[NewState](f : State => NewState) : EventReaction[WidgetTask, NewState, UpEvent, ParentUpEvent] =
    EventReaction(f(newState), parentEvent, ios)
  end mapState
end EventReaction
