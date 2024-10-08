package me.katze.gui4s.widget
package library

import stateful.{Path, TaskFinished}
import impl.WidgetTaskImpl
import cats.*
import cats.syntax.all.given
import library.lowlevel.{WidgetLibrary, WidgetLibraryImpl}


type LayoutPlacementStrategy[Widget, PlacedWidget, PlacementEffect[+_], ChildrenMeta] =  List[Widget] => PlacementEffect[List[(PlacedWidget, ChildrenMeta)]]

trait LayoutLibrary[-WL <: WidgetLibrary, -ChildrenMeta]:
  def layout[Event]
      (using lib : WL)
      (
        children : List[lib.Widget[Event]],
        placementStrategy : LayoutPlacementStrategy[lib.Widget[Event], lib.PlacedWidget[Event, lib.SystemEvent], lib.PlacementEffect, ChildrenMeta]
      ) : lib.Widget[Event]
end LayoutLibrary

type WidgetTaskT[F[+_]] = [T] =>> WidgetTaskImpl[F, T]

given layoutLibraryImpl[F[+_], Draw, PlacementEffect[+_], ChildrenMeta, DownEvent >: TaskFinished](using LayoutDraw[Draw, ChildrenMeta]): LayoutLibrary[WidgetLibraryImpl[F, Draw, PlacementEffect, WidgetTaskT[F], DownEvent], ChildrenMeta] with
  override def layout[Event]
    (using lib : WidgetLibraryImpl[F, Draw, PlacementEffect, WidgetTaskT[F], DownEvent])
    (
      children         : List[lib.Widget[Event]],
      placementStrategy: LayoutPlacementStrategy[lib.Widget[Event], lib.PlacedWidget[Event, lib.SystemEvent], lib.PlacementEffect, ChildrenMeta]
    ): lib.Widget[Event] =

    class LayoutWidget(
                        children : List[(lib.PlacedWidget[Event, lib.SystemEvent], ChildrenMeta)],
                        placeFree: List[lib.Widget[Event]] => lib.Widget[Event]
                      ) extends me.katze.gui4s.widget.PlacedWidget[lib.Draw, lib.WidgetTask[Any], lib.FreeWidget, Event, lib.SystemEvent]:

      override def handleDownEvent(event: lib.SystemEvent): EventResult[lib.WidgetTask[Any], lib.FreeWidget[Event, lib.SystemEvent], Event] =
        val eventResults = children.map(_._1.handleDownEvent(event))
        val tasks = eventResults.flatMap(_.ios)
        val upEvents = eventResults.flatMap(_.upEvent)
        val newChildren = eventResults.map(_.widget)
        EventResult(
          lib.placementIsEffect.flatMap(placeFree(newChildren))(_.mergeWithState(childrenStates)),
          upEvents,
          tasks
        )
      end handleDownEvent
      
      override def asFree: lib.FreeWidget[Event, lib.SystemEvent] =
        placeFree(children.map(_._1.asFree))
      end asFree

      override def mergeWithState(oldState: Map[String, Any]): lib.FreeWidget[Event, lib.SystemEvent] =
        placeFree(children.map(_._1.mergeWithState(oldState)))
      end mergeWithState

      override def childrenStates: Map[String, Any] =
        children.flatMap(_._1.childrenStates).toMap
      end childrenStates

      override def draw: lib.Draw =
        summon[LayoutDraw[Draw, ChildrenMeta]].drawChildren(children.map(child => (child._1.draw, child._2)))
      end draw

      override def filterDeadPaths(
                                    currentPath: Path,
                                    alive      : Set[Path]
                                  ): Set[Path] =
        children.foldLeft(alive)((res, child) => child._1.filterDeadPaths(currentPath, res))
      end filterDeadPaths
    end LayoutWidget

    lib.placementIsEffect.map(placementStrategy(children))(placedChilren => lib.constructRealWidget(LayoutWidget(placedChilren, layout(using lib)(_, placementStrategy))))
  end layout
end layoutLibraryImpl

