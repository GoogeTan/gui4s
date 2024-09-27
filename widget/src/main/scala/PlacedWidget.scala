package me.katze.gui4s.widget

import stateful.Path

trait PlacedWidget[+Draw, +WidgetTask, +FreeWidget[+_, -_], +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : EventResult[WidgetTask, FreeWidget[RaiseableEvent, HandleableEvent], RaiseableEvent]
  
  def asFree : FreeWidget[RaiseableEvent, HandleableEvent]

  def mergeWithState(oldState : Map[String, Any]) : FreeWidget[RaiseableEvent, HandleableEvent]

  def childrenStates : Map[String, Any]
  
  def draw : Draw
  
  def filterDeadPaths(alive : Set[Path]) : Set[Path]
end PlacedWidget