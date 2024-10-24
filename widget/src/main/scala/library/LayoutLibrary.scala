package me.katze.gui4s.widget
package library

import me.katze.gui4s.widget.stateful.{BiMonad, Path, given}
import cats.*
import cats.syntax.all.given
import library.lowlevel.{WidgetLibrary, WidgetLibraryGeneric}

type LayoutPlacementStrategy[Widget, PlacedWidget, PlacementEffect[+_], ChildrenMeta] =  List[Widget] => PlacementEffect[List[(PlacedWidget, ChildrenMeta)]]

trait LayoutLibrary[-WL <: WidgetLibrary, -ChildrenMeta]:
  def layout[Event]
      (using lib : WL)
      (
        children : List[lib.Widget[Event]],
        placementStrategy : LayoutPlacementStrategy[lib.Widget[Event], lib.PlacedWidget[Event, lib.SystemEvent], lib.PlacementEffect, ChildrenMeta]
      ) : lib.Widget[Event]
end LayoutLibrary

given layoutLibraryImpl[
  WL <: WidgetLibraryGeneric[Update, Place, Draw, PlacedWidgetTree, DownEvent],
  Update[+_, +_] : BiMonad,
  Draw,
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Draw, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  Place[+_] : FlatMap,
  ChildrenMeta,
  DownEvent
](
  using
    ld: LayoutDraw[Draw, ChildrenMeta]
): LayoutLibrary[WL, ChildrenMeta] with
  override def layout[Event]
    (using lib : WL)
    (
      children         : List[lib.Widget[Event]],
      placementStrategy: LayoutPlacementStrategy[lib.Widget[Event], lib.PlacedWidget[Event, lib.SystemEvent], lib.PlacementEffect, ChildrenMeta]
    ): lib.Widget[Event] =
    lib.placementIsEffect.map(placementStrategy(children))(
      placedChilren => 
        lib.constructRealWidget(LayoutWidget(placedChilren, layout(_, placementStrategy)))
    )
  end layout
end layoutLibraryImpl

final class LayoutWidget[
  UpdateF[+_, +_] : BiMonad,
  DrawF,
  PlaceF[+_] : FlatMap,
  PlacedWidget[+RaisesEvent, -HandlesEvent] <: me.katze.gui4s.widget.PlacedWidget[UpdateF, DrawF, [A, B] =>> PlaceF[PlacedWidget[A, B]], RaisesEvent, HandlesEvent],
  UpEvent,
  DownEvent,
  ChildrenMeta
](
    children : List[(PlacedWidget[UpEvent, DownEvent], ChildrenMeta)],
    placeFree: List[PlaceF[PlacedWidget[UpEvent, DownEvent]]] => PlaceF[PlacedWidget[UpEvent, DownEvent]]
)(
  using LayoutDraw[DrawF, ChildrenMeta]
) extends me.katze.gui4s.widget.PlacedWidget[UpdateF, DrawF, [A, B] =>> PlaceF[PlacedWidget[A, B]], UpEvent, DownEvent]:

  override def handleDownEvent(event: DownEvent): UpdateF[PlaceF[PlacedWidget[UpEvent, DownEvent]], UpEvent] =
    children
      .traverse[[T] =>> UpdateF[T, UpEvent], PlaceF[PlacedWidget[UpEvent, DownEvent]]](_._1.handleDownEvent(event))
      .map(newChildren => placeFree(newChildren).flatMap(_.mergeWithState(childrenStates)))
  end handleDownEvent

  override def asFree: PlaceF[PlacedWidget[UpEvent, DownEvent]] =
    placeFree(children.map(_._1.asFree))
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): PlaceF[PlacedWidget[UpEvent, DownEvent]] =
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
