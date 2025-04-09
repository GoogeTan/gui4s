package me.katze.gui4s.widget

import stateful.{BiMonad, Path}

import cats.FlatMap

final case class LaunchedEffectWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition,
  UpEvent,
  DownEvent,
  Keys
](
  name : String,
  keys : Keys,
  taskOnChange : Path => Recomposition,
  nothingToDo : Recomposition,
  nothingToDraw : Draw,
  override val asFree: Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]],
  stateTypeMismatchRecompositionError: (Any, Path) => Recomposition,
  stateTypeMismatchPlaceError: (Any, Path) => Place[Nothing]
) extends Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]:

  private final case class LaunchedEffectState(oldKeys: Keys)

  override def handleDownEvent(
                                pathToParent: Path, event: DownEvent
                              ): Update[Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]], UpEvent] =
    asFree.asMonad
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
                              ): Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]] =
    oldState.get(name) match
      case Some(StateTree(LaunchedEffectState(oldKeys), _, _)) =>
        copy(keys = oldKeys).asFree
      case Some(StateTree(value, _, _)) =>
        stateTypeMismatchPlaceError(value, pathToParent.appendLast(this.name))
      case None =>
        asFree
    end match
  end mergeWithState

  override def aliveWidgets(currentPath: Path): Set[Path] =
    Set(currentPath.appendLast(name))
  end aliveWidgets

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    Map(name -> StateTree[Recomposition](LaunchedEffectState(keys), nothingToDo, Map()))
  end childrenStates

  override def draw: Draw = nothingToDraw
end LaunchedEffectWidget
