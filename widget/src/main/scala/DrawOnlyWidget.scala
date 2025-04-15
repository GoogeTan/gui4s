package me.katze.gui4s.widget

import library.Empty
import stateful.Path

import cats.syntax.all.*
import cats.{Applicative, FlatMap}

def drawOnlyWidget[
  Update[+_] : Applicative,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Empty as E
](asFreeIn: Place[Widget[Update, Draw, Place, Recomposition, Any]], drawIn: Draw): Widget[Update, Draw, Place, Recomposition, Any] =
  case object DrawOnlyWidget extends Widget[Update, Draw, Place, Recomposition, Any]:
    override def handleDownEvent(pathToParent: Path, event: Any): Update[Place[Widget[Update, Draw, Place, Recomposition, Any]]] = asFree.pure

    override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, Any]] = asFree

    override def childrenStates: Map[String, StateTree[Recomposition]] = Map()

    override val asFree: Place[Widget[Update, Draw, Place, Recomposition, Any]] = asFreeIn
    
    override val draw  : Draw = drawIn

    override def aliveWidgets(currentPath: Path): Set[Path] = Set()
    
    override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition = E.empty
  end DrawOnlyWidget
  
  DrawOnlyWidget
end drawOnlyWidget
