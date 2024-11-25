package me.katze.gui4s.widget
package library

import stateful.{BiMonad, Path}

import cats.FlatMap

final case class LaunchedEffectWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition,
  UpEvent,
  DownEvent
](
  name : String,
  keys : List[Any],
  taskOnChange : Path => Recomposition,
  nothingToDo : Recomposition,
  nothingToDraw : Draw,
  override val asFree: Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]]
) extends Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]:
  override def handleDownEvent(
                                pathToParent: Path, event: DownEvent
                              ): Update[Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]], UpEvent] =
    asFree.asMonad
  end handleDownEvent

  override def recomposed(
                            currentPath: Path,
                            states     : Map[String, StateTree[Recomposition]]
                          ): Recomposition =
    states.get(name) match
      case Some(value) =>
        val oldKeys = value.asInstanceOf[List[Any]]
        if keys != oldKeys then
          taskOnChange(currentPath)
        else
          nothingToDo
        end if
      case None =>
        taskOnChange(currentPath)
    end match
  end recomposed

  override def mergeWithState(
                                pathToParent: Path,
                                oldState    : Map[String, StateTree[Recomposition]]
                              ): Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]] =
    oldState.get(name) match
      case Some(value) =>
        val oldKeys = value.asInstanceOf[List[Any]]
        copy(keys = oldKeys).asFree
      case None =>
        asFree
    end match

  override def aliveWidgets(currentPath: Path): Set[Path] =
    Set(currentPath.appendLast(name))
  end aliveWidgets

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    Map(name -> StateTree[Recomposition](keys, nothingToDo, Map()))
  end childrenStates

  override def draw: Draw = nothingToDraw
end LaunchedEffectWidget
