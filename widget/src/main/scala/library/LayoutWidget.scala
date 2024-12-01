package me.katze.gui4s.widget
package library

import stateful.{BiMonad, Path, given}

import cats.{FlatMap, Monoid}
import cats.syntax.all.*

final class LayoutWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  UpEvent,
  DownEvent,
  ChildrenMeta
](
    children : List[(Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent], ChildrenMeta)],
    placeChildren: List[Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]]] => Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]]
)(
    using LayoutDraw[Draw, ChildrenMeta]
) extends me.katze.gui4s.widget.Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]:

  override def handleDownEvent(pathToParent: Path, event: DownEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]], UpEvent] =
    children
      .traverse[[T] =>> Update[T, UpEvent], Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]]](_._1.handleDownEvent(pathToParent, event))
      .map(newChildren => placeChildren(newChildren).flatMap(_.mergeWithState(pathToParent, childrenStates)))
  end handleDownEvent

  override def asFree: Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]] =
    placeChildren(children.map(_._1.asFree))
  end asFree

  override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent]] =
    placeChildren(children.map(_._1.mergeWithState(pathToParent, oldState)))
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    children.flatMap(_._1.childrenStates).toMap
  end childrenStates

  override def draw: Draw =
    summon[LayoutDraw[Draw, ChildrenMeta]].drawChildren(children.map(child => (child._1.draw, child._2)))
  end draw

  override def aliveWidgets(currentPath: Path): Set[Path] =
    children.foldMap(_._1.aliveWidgets(currentPath))
  end aliveWidgets

  override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition =
    children.foldMap((child, meta) => child.recomposed(currentPath, states))
  end recomposed
end LayoutWidget

