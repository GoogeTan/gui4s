package me.katze.gui4s.widget
package library

import cats.{FlatMap, Monad}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.stateful.{BiMonad, Path}

def drawOnlyWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap
](asFreeIn: Place[PlacedWidget[Update, Draw, Place, Nothing, Any]], drawIn: Draw): PlacedWidget[Update, Draw, Place,Nothing, Any] =
  case object DrawOnlyWidget extends PlacedWidget[Update, Draw, Place, Nothing, Any]:
    override def handleDownEvent(event: Any): Update[Place[PlacedWidget[Update, Draw, Place,Nothing, Any]], Nothing] = asFree.asMonad

    override def mergeWithState(oldState: Map[String, Any]): Place[PlacedWidget[Update, Draw, Place,Nothing, Any]] = asFree

    override def childrenStates: Map[String, Any] = Map()

    override def filterDeadPaths(
                                  currentPath: Path,
                                  alive      : Set[Path]
                                ): Set[Path] = alive

    override val asFree: Place[PlacedWidget[Update, Draw, Place,Nothing, Any]] = asFreeIn
    override val draw  : Draw = drawIn
  end DrawOnlyWidget
  
  DrawOnlyWidget
end drawOnlyWidget
