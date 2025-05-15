package me.katze.gui4s.widget
package drawonly

import catnip.Empty
import cats.syntax.all.*
import cats.{Applicative, FlatMap}

def drawOnlyWidget[
  Update[+_] : Applicative,
  Draw,
  Place[+_],
  Recomposition : Empty as E
](asFreeIn: Place[Widget[Update, Draw, Place, Recomposition, Any]], drawIn: Draw): Widget[Update, Draw, Place, Recomposition, Any] =
  case object DrawOnlyWidget extends Widget[Update, Draw, Place, Recomposition, Any]:
    override def handleDownEvent(pathToParent: Path, event: Any): Update[Place[Widget[Update, Draw, Place, Recomposition, Any]]] = asUnplaced.pure

    override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, Any]] = asUnplaced

    override def childrenStates: Map[String, StateTree[Recomposition]] = Map()

    override val asUnplaced: Place[Widget[Update, Draw, Place, Recomposition, Any]] = asFreeIn
    
    override val draw  : Draw = drawIn

    override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition = E.empty
  end DrawOnlyWidget
  
  DrawOnlyWidget
end drawOnlyWidget
