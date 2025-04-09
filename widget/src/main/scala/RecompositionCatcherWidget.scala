package me.katze.gui4s.widget

import library.Empty
import stateful.{BiMonad, Path}

final case class RecompositionCatcherWidget[
  +Update[+_, +_] : BiMonad,
  +Draw : Empty as E,
  +Place[+_],
  Recomposition,
](
  override val asFree: Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]],
  onRecomposition : Recomposition 
) extends Widget[Update, Draw, Place, Recomposition, Nothing, Any]:
  override def recomposed(currentPath: Path, states: Map[String, StateTree[Recomposition]]): Recomposition =
    onRecomposition
  end recomposed

  override def aliveWidgets(currentPath: Path): Set[Path] = Set()

  override def handleDownEvent(pathToParent: Path, event: Any): Update[Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]], Nothing] =
    asFree.asMonad
  end handleDownEvent

  override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, Nothing, Any]] =
    asFree
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] = Map()

  override def draw: Draw = E.empty
end RecompositionCatcherWidget


