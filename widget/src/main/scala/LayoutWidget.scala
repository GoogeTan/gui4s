package me.katze.gui4s.widget

import library.LayoutDraw
import stateful.Path

import cats.syntax.all.*
import cats.{FlatMap, Monad, Monoid}

final class LayoutWidget[
  Update[+_] : Monad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  DownEvent,
  ChildrenMeta
](
    children : List[(Widget[Update, Draw, Place, Recomposition, DownEvent], ChildrenMeta)],
    placeChildren: List[Place[Widget[Update, Draw, Place, Recomposition, DownEvent]]] => Place[Widget[Update, Draw, Place, Recomposition, DownEvent]]
)(
    using LD : LayoutDraw[Draw, ChildrenMeta]
) extends me.katze.gui4s.widget.Widget[Update, Draw, Place, Recomposition, DownEvent]:

  override def handleDownEvent(pathToParent: Path, event: DownEvent): Update[Place[Widget[Update, Draw, Place, Recomposition, DownEvent]]] =
    children
      .traverse(_._1.handleDownEvent(pathToParent, event))
      .map(newChildren => placeChildren(newChildren).flatMap(_.mergeWithState(pathToParent, childrenStates)))
  end handleDownEvent

  override def asUnplaced: Place[Widget[Update, Draw, Place, Recomposition, DownEvent]] =
    placeChildren(children.map(_._1.asUnplaced))
  end asUnplaced

  override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[Update, Draw, Place, Recomposition, DownEvent]] =
    placeChildren(children.map(_._1.mergeWithState(pathToParent, oldState)))
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] =
    children.flatMap(_._1.childrenStates).toMap
  end childrenStates

  override def draw: Draw =
    LD.drawChildren(children.map(child => (child._1.draw, child._2)))
  end draw

  override def recomposed(currentPath : Path, states : Map[String, StateTree[Recomposition]]): Recomposition =
    children.foldMap((child, _) => child.recomposed(currentPath, states))
  end recomposed
end LayoutWidget

