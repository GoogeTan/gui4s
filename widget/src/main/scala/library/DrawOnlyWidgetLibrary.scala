package me.katze.gui4s.widget
package library

import cats.{FlatMap, Monad}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.stateful.{BiMonad, Path}

def drawOnlyWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  LeftComposition : Empty
](asFreeIn: Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]], drawIn: Draw): Widget[Update, Draw, Place, LeftComposition, Nothing, Any] =
  case object DrawOnlyWidget extends Widget[Update, Draw, Place, LeftComposition, Nothing, Any]:
    override def handleDownEvent(pathToParent: Path, event: Any): Update[Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]], Nothing] = asFree.asMonad

    override def mergeWithState(pathToParent: Path, oldState: Map[String, Any]): Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]] = asFree

    override def childrenStates: Map[String, Any] = Map()

    override val asFree: Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]] = asFreeIn
    
    override val draw  : Draw = drawIn

    override def aliveWidgets(currentPath: Path): Set[Path] = Set()
    
    override def recomposed(currentPath : Path): LeftComposition = summon[Empty[LeftComposition]].empty
  end DrawOnlyWidget
  
  DrawOnlyWidget
end drawOnlyWidget
