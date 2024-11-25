package me.katze.gui4s.widget
package library

import me.katze.gui4s.widget.stateful.{BiMonad, Path, given}
import cats.*
import cats.syntax.all.given

type LayoutPlacementStrategy[Widget, PlacedWidget, PlacementEffect[+_], ChildrenMeta] =  List[Widget] => PlacementEffect[List[(PlacedWidget, ChildrenMeta)]]

trait LayoutLibrary[Place[+_], Widget[+_], -ChildrenMeta]:
  def layout[Event]
      (
        children : List[Place[Widget[Event]]],
        placementStrategy : LayoutPlacementStrategy[Place[Widget[Event]], Widget[Event], Place, ChildrenMeta]
      ) : Place[Widget[Event]]
end LayoutLibrary

given layoutLibraryImpl[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  ChildrenMeta,
  DownEvent
](
  using
    ld: LayoutDraw[Draw, ChildrenMeta]
): LayoutLibrary[Place, [A] =>> Widget[Update, Draw, Place, Recomposition, A, DownEvent], ChildrenMeta] with
  type Widget[A] = me.katze.gui4s.widget.Widget[Update, Draw, Place, Recomposition, A, DownEvent]

  override def layout[Event](
                              children         : List[Place[Widget[Event]]],
                              placementStrategy: LayoutPlacementStrategy[Place[Widget[Event]], Widget[Event], Place, ChildrenMeta]
                            ): Place[Widget[Event]] =
    placementStrategy(children).map(LayoutWidget(_, layout(_, placementStrategy)))
  end layout
end layoutLibraryImpl

final class LayoutWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  LeftComposition : Monoid,
  UpEvent,
  DownEvent,
  ChildrenMeta
](
    children : List[(Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent], ChildrenMeta)],
    placeChildren: List[Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]]] => Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]]
)(
  using LayoutDraw[Draw, ChildrenMeta]
) extends me.katze.gui4s.widget.Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]:

  override def handleDownEvent(pathToParent: Path, event: DownEvent): Update[Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]], UpEvent] =
    children
      .traverse[[T] =>> Update[T, UpEvent], Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]]](_._1.handleDownEvent(pathToParent, event))
      .map(newChildren => placeChildren(newChildren).flatMap(_.mergeWithState(pathToParent, childrenStates)))
  end handleDownEvent

  override def asFree: Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]] =
    placeChildren(children.map(_._1.asFree))
  end asFree

  override def mergeWithState(pathToParent: Path, oldState: Map[String, Any]): Place[Widget[Update, Draw, Place, LeftComposition, UpEvent, DownEvent]] =
    placeChildren(children.map(_._1.mergeWithState(pathToParent, oldState)))
  end mergeWithState

  override def childrenStates: Map[String, Any] =
    children.flatMap(_._1.childrenStates).toMap
  end childrenStates

  override def draw: Draw =
    summon[LayoutDraw[Draw, ChildrenMeta]].drawChildren(children.map(child => (child._1.draw, child._2)))
  end draw

  override def aliveWidgets(
                              currentPath: Path,
                            ): Set[Path] =
    children.foldMap(_._1.aliveWidgets(currentPath))
  end aliveWidgets
  
  override def recomposed(currentPath : Path): LeftComposition =
    children.foldMap(_._1.recomposed(currentPath))
  end recomposed
end LayoutWidget
