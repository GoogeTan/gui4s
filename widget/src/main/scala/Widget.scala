package me.katze.gui4s.widget

import stateful.Path


trait Widget[+Update[+_], +Draw, +Place[+_], Recomposition, -HandleableEvent]:
  def handleDownEvent(pathToParent: Path, event: HandleableEvent) : Update[Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]]]
  
  def asUnplaced : Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]]

  def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]) : Place[Widget[Update, Draw, Place, Recomposition, HandleableEvent]]

  def childrenStates : Map[String, StateTree[Recomposition]]
  
  def draw : Draw
  
  def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]) : Recomposition
end Widget
