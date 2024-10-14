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
  Update[+_, +_] : BiMonad,
  Merge[+_] : Monad,
  Draw,
  WidgetTask,
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Merge, Draw, WidgetTask, [A, B] =>> Place[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  Place[+_] : Monad,
  ChildrenMeta,
  DownEvent
](
  using
    ld: LayoutDraw[Draw, ChildrenMeta],
    swapEffects: [A] => Place[Merge[A]] => Merge[Place[A]],
    runMerge: [T] => Merge[T] => Update[T, Nothing]
): LayoutLibrary[WidgetLibraryGeneric[Update, Merge, Place, Draw, PlacedWidgetTree, WidgetTask, DownEvent], ChildrenMeta] with
  override def layout[Event]
    (using lib : WidgetLibraryGeneric[Update, Merge, Place, Draw, PlacedWidgetTree, WidgetTask, DownEvent])
    (
      children         : List[lib.Widget[Event]],
      placementStrategy: LayoutPlacementStrategy[lib.Widget[Event], lib.PlacedWidget[Event, lib.SystemEvent], lib.PlacementEffect, ChildrenMeta]
    ): lib.Widget[Event] =
    
    lib.placementIsEffect.map(placementStrategy(children))(
      placedChilren => 
        lib.constructRealWidget(LayoutWidget(placedChilren, layout(_, placementStrategy), swapEffects, runMerge))
    )
  end layout
end layoutLibraryImpl

final class LayoutWidget[
  UpdateF[+_, +_] : BiMonad,
  MergeF[+_] : Monad,
  DrawF,
  PlaceF[+_] : Monad,
  PlacedWidget[+RaisesEvent, -HandlesEvent] <: me.katze.gui4s.widget.PlacedWidget[UpdateF, MergeF, DrawF, WidgetTask, [A, B] =>> PlaceF[PlacedWidget[A, B]], RaisesEvent, HandlesEvent],
  WidgetTask,
  UpEvent,
  DownEvent,
  ChildrenMeta
](
    children : List[(PlacedWidget[UpEvent, DownEvent], ChildrenMeta)],
    placeFree: List[PlaceF[PlacedWidget[UpEvent, DownEvent]]] => PlaceF[PlacedWidget[UpEvent, DownEvent]],
    swapEffects: [A] => PlaceF[MergeF[A]] => MergeF[PlaceF[A]],
    runMerge: [T] => MergeF[T] => UpdateF[T, Nothing]
)(
  using LayoutDraw[DrawF, ChildrenMeta]
) extends me.katze.gui4s.widget.PlacedWidget[UpdateF, MergeF, DrawF, WidgetTask, [A, B] =>> PlaceF[PlacedWidget[A, B]], UpEvent, DownEvent]:

  override def handleDownEvent(event: DownEvent): UpdateF[PlaceF[PlacedWidget[UpEvent, DownEvent]], UpEvent] =
    children.traverse[[T] =>> UpdateF[T, UpEvent], PlaceF[PlacedWidget[UpEvent, DownEvent]]](_._1.handleDownEvent(event))
      .flatMap(newChildren =>
        // TODO Отрефакторить это чудо. Надо разделить все 4 эффекта, обобщить операцию над ними(такая же есть в стейтфуле).
        val a : PlaceF[MergeF[PlaceF[PlacedWidget[UpEvent, DownEvent]]]] = placeFree(newChildren).map(_.mergeWithState(childrenStates))
        val b : MergeF[PlaceF[PlacedWidget[UpEvent, DownEvent]]] = swapEffects(a).map(_.flatten)
        val c : UpdateF[PlaceF[PlacedWidget[UpEvent, DownEvent]], Nothing] = runMerge(b)
        c,
      )
  end handleDownEvent

  override def asFree: PlaceF[PlacedWidget[UpEvent, DownEvent]] =
    placeFree(children.map(_._1.asFree))
  end asFree

  override def mergeWithState(oldState: Map[String, Any]): MergeF[PlaceF[PlacedWidget[UpEvent, DownEvent]]] =
    children.traverse(_._1.mergeWithState(oldState)).map(placeFree)
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
