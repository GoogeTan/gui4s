package me.katze.gui4s.widget
package library

import cats.FlatMap
import cats.syntax.all.*
import me.katze.gui4s.widget.stateful.{BiMonad, Path}

def drawOnlyWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Empty
](asFreeIn: Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]], drawIn: Draw): Widget[Update, Draw, Place, Recomposition, Nothing, Any] =
  case object DrawOnlyWidget extends Widget[Update, Draw, Place, Recomposition, Nothing, Any]:
    override def handleDownEvent(pathToParent: Path, event: Any): Update[Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]], Nothing] = asFree.asMonad

    override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]] = asFree

    override def childrenStates: Map[String, StateTree[Recomposition]] = Map()

    override val asFree: Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]] = asFreeIn
    
    override val draw  : Draw = drawIn

    override def aliveWidgets(currentPath: Path): Set[Path] = Set()
    
    override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition = summon[Empty[Recomposition]].empty
  end DrawOnlyWidget
  
  DrawOnlyWidget
end drawOnlyWidget
