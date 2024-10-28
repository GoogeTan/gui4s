package me.katze.gui4s.widget

import stateful.Path

trait Widget[+Update[+_, +_], +Draw, +Place[+_], +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : Update[Place[Widget[Update, Draw, Place, RaiseableEvent, HandleableEvent]], RaiseableEvent]
  
  def asFree : Place[Widget[Update, Draw, Place, RaiseableEvent, HandleableEvent]]

  def mergeWithState(oldState : Map[String, Any]) : Place[Widget[Update, Draw, Place, RaiseableEvent, HandleableEvent]]

  def childrenStates : Map[String, Any]
  
  def draw : Draw
  
  def filterDeadPaths(currentPath : Path, alive : Set[Path]) : Set[Path]

end Widget
