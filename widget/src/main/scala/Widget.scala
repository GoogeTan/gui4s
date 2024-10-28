package me.katze.gui4s.widget

import stateful.Path

trait Widget[+Update[+_, +_], +Draw, +Place[+_], Recomposition, +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(event : HandleableEvent) : Update[Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]], RaiseableEvent]
  
  def asFree : Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]]

  def mergeWithState(oldState : Map[String, Any]) : Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]]

  def childrenStates : Map[String, Any]
  
  def draw : Draw
  
  def aliveWidgets(currentPath : Path) : Set[Path]

  def recomposed(currentPath : Path) : Recomposition
end Widget
