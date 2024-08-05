package me.katze.gui4s.example


trait PlacedWidget[+WidgetTask, +FreeWidget[+_, -_], +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : EventResult[WidgetTask, FreeWidget[RaiseableEvent, HandleableEvent], RaiseableEvent]
  def asFree : FreeWidget[RaiseableEvent, HandleableEvent]

  def mergeWithState(oldState : Map[String, Any]) : FreeWidget[RaiseableEvent, HandleableEvent]

  def childrenStates : Map[String, Any]
  
  def prettyString : String
end PlacedWidget
