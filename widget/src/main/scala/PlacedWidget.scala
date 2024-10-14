package me.katze.gui4s.widget

import stateful.Path

trait PlacedWidget[+Update[+_, +_], +Merge[+_], +Draw, +WidgetTask, +FreeWidget[+_, -_], +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : Update[FreeWidget[RaiseableEvent, HandleableEvent], RaiseableEvent]
  
  def asFree : FreeWidget[RaiseableEvent, HandleableEvent]

  def mergeWithState(oldState : Map[String, Any]) : Merge[FreeWidget[RaiseableEvent, HandleableEvent]]

  def childrenStates : Map[String, Any]
  
  def draw : Draw
  
  def filterDeadPaths(currentPath : Path, alive : Set[Path]) : Set[Path]
end PlacedWidget
