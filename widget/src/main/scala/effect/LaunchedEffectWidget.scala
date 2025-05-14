package me.katze.gui4s.widget
package effect

import cats.syntax.all.*
import cats.{FlatMap, Monad}

final case class LaunchedEffectWidget[
  Update[+_] : Monad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition,
  Keys
](
    name : String,
    keys : Keys,
    taskOnChange : Path => Recomposition,
    nothingToDo : Recomposition,
    nothingToDraw : Draw,
    override val asUnplaced: Place[Widget[Update, Draw, Place, Recomposition, Any]],
    stateTypeMismatchRecompositionError: (Any, Path) => Recomposition,
    stateTypeMismatchPlaceError: (Any, Path) => Place[Nothing]
) extends Widget[Update, Draw, Place, Recomposition, Any]:
  private final case class LaunchedEffectState(oldKeys: Keys)

  override def handleDownEvent(
                                pathToParent: Path, event: Any
                              ): Update[Place[Widget[Update, Draw, Place, Recomposition, Any]]] =
    asUnplaced.pure
  end handleDownEvent

  override def recomposed(
                            pathToParent: Path,
                            states      : Map[String, StateTree[Recomposition]]
                          ): Recomposition =
    states.get(name) match
      case Some(StateTree(LaunchedEffectState(oldKeys), _, _)) =>
        if keys != oldKeys then
          taskOnChange(pathToParent)
        else
          nothingToDo
        end if
      case Some(StateTree(value, _, _)) =>
        stateTypeMismatchRecompositionError(value, pathToParent.appendLast(this.name))
      case None =>
        taskOnChange(pathToParent)
    end match
  end recomposed

  override def mergeWithState(
                                pathToParent: Path,
                                oldState    : Map[String, StateTree[Recomposition]]
                              ): Place[Widget[Update, Draw, Place, Recomposition, Any]] =
    oldState.get(name) match
      case Some(StateTree(LaunchedEffectState(oldKeys), _, _)) =>
        copy(keys = oldKeys).asUnplaced
      case Some(StateTree(value, _, _)) =>
        stateTypeMismatchPlaceError(value, pathToParent.appendLast(this.name))
      case None =>
        asUnplaced
    end match
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    Map(name -> StateTree[Recomposition](LaunchedEffectState(keys), nothingToDo, Map()))
  end childrenStates

  override def draw: Draw = nothingToDraw
end LaunchedEffectWidget
