package me.katze.gui4s.example
package stateful

trait State[WidgetTask, RaiseableEvent, +ParentRaisableEvent, +Tree]:
  def render : Tree
  def handleEvent(event : RaiseableEvent) : EventReaction[WidgetTask, State[WidgetTask, RaiseableEvent, ParentRaisableEvent, Tree], RaiseableEvent, ParentRaisableEvent]
  def state : Any
  def prettyString : String
  def wergeWithOldState(oldState : Any) : State[WidgetTask, RaiseableEvent, ParentRaisableEvent, Tree]
end State