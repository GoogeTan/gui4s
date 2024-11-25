package me.katze.gui4s.widget

import stateful.Path


trait Widget[+Update[+_, +_], +Draw, +Place[+_], Recomposition, +RaiseableEvent, -HandleableEvent]:
  def handleDownEvent(pathToParent: Path, event: HandleableEvent) : Update[Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]], RaiseableEvent]
  
  def asFree : Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]]

  def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]) : Place[Widget[Update, Draw, Place, Recomposition, RaiseableEvent, HandleableEvent]]

  def childrenStates : Map[String, StateTree[Recomposition]]
  
  def draw : Draw
  
  def aliveWidgets(currentPath : Path) : Set[Path]

  def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]) : Recomposition
end Widget
