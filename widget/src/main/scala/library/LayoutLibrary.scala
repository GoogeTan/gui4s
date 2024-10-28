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
  ChildrenMeta,
  DownEvent
](
  using
    ld: LayoutDraw[Draw, ChildrenMeta]
): LayoutLibrary[Place, [A] =>> Widget[Update, Draw, Place, A, DownEvent], ChildrenMeta] with
  type Widget[A] = me.katze.gui4s.widget.Widget[Update, Draw, Place, A, DownEvent]

  override def layout[Event](
                              children         : List[Place[Widget[Event]]],
                              placementStrategy: LayoutPlacementStrategy[Place[Widget[Event]], Widget[Event], Place, ChildrenMeta]
                            ): Place[Widget[Event]] =
    placementStrategy(children).map(LayoutWidget(_, layout(_, placementStrategy)))
  end layout
end layoutLibraryImpl

final class LayoutWidget[
  UpdateF[+_, +_] : BiMonad,
  DrawF,
  PlaceF[+_] : FlatMap,
  UpEvent,
  DownEvent,
  ChildrenMeta
](
   children : List[(Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent], ChildrenMeta)],
   placeFree: List[PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]]] => PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]]
)(
  using LayoutDraw[DrawF, ChildrenMeta]
) extends me.katze.gui4s.widget.Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]:

  override def handleDownEvent(event: DownEvent): UpdateF[PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]], UpEvent] =
    children
      .traverse[[T] =>> UpdateF[T, UpEvent], PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]]](_._1.handleDownEvent(event))
      .map(newChildren => placeFree(newChildren).flatMap(_.mergeWithState(childrenStates)))
  end handleDownEvent

  override def asFree: PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]] =
    placeFree(children.map(_._1.asFree))
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): PlaceF[Widget[UpdateF, DrawF, PlaceF, UpEvent, DownEvent]] =
    placeFree(children.map(_._1.mergeWithState(oldState)))
  end mergeWithState

  override def childrenStates: Map[String, Any] =
    children.flatMap(_._1.childrenStates).toMap
  end childrenStates

  override def draw: DrawF =
    summon[LayoutDraw[DrawF, ChildrenMeta]].drawChildren(children.map(child => (child._1.draw, child._2)))
  end draw

  override def filterDeadPaths(
                                currentPath: Path,
                                alive      : Set[Path]
                              ): Set[Path] =
    children.foldLeft(alive)((res, child) => child._1.filterDeadPaths(currentPath, res))
  end filterDeadPaths
end LayoutWidget
