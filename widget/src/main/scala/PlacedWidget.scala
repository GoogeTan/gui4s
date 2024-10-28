package me.katze.gui4s.widget

import stateful.Path

trait PlacedWidget[+Update[+_, +_], +Draw, +Place[+_], +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : Update[Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent]
  
  def asFree : Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]]

  def mergeWithState(oldState : Map[String, Any]) : Place[PlacedWidget[Update, Draw, Place, RaiseableEvent, HandleableEvent]]

  def childrenStates : Map[String, Any]
  
  def draw : Draw
  
  def filterDeadPaths(currentPath : Path, alive : Set[Path]) : Set[Path]
end PlacedWidget
