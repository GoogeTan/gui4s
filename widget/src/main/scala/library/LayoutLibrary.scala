package me.katze.gui4s.widget
package library

import stateful.Path

import cats.*
import cats.syntax.all.given

trait LayoutLibrary extends WidgetLibrary:
  type ChildrenMeta
  override type PlacedWidget[+A, -B] <: me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], FreeWidget, A, B]
  
  given layoutIsDrawable: LayoutDraw[Draw, ChildrenMeta]
  
  private class LayoutWidget[Event](
                                      children : List[(PlacedWidget[Event, SystemEvent], ChildrenMeta)],
                                      placeFree : List[Widget[Event]] => Widget[Event]
                                    )(
                                      using LD : LayoutDraw[Draw, ChildrenMeta]
                                    ) extends me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], FreeWidget, Event, SystemEvent]:
    override def handleDownEvent(event: SystemEvent): EventResult[WidgetTask[Any], FreeWidget[Event, SystemEvent], Event] =
      val eventResults = children.map(_._1.handleDownEvent(event))
      val tasks = eventResults.flatMap(_.ios)
      val upEvents = eventResults.flatMap(_.upEvent)
      val newChildren = eventResults.map(_.widget)
      EventResult(
        Monad[PlacementEffect].flatMap(placeFree(newChildren))(_.mergeWithState(childrenStates)),
        upEvents,
        tasks
      )
    end handleDownEvent


    override def asFree: FreeWidget[Event, SystemEvent] =
      placeFree(children.map(_._1.asFree))
    end asFree

    override def mergeWithState(oldState: Map[String, Any]): FreeWidget[Event, SystemEvent] =
      placeFree(children.map(_._1.mergeWithState(oldState)))
    end mergeWithState

    override def childrenStates: Map[String, Any] =
      children.flatMap(_._1.childrenStates).toMap
    end childrenStates

    override def draw: Draw =
      LD.drawChildren(children.map(child => (child._1.draw, child._2)))
    end draw

    override def filterDeadPaths(
                                  currentPath: Path,
                                  alive      : Set[Path]
                                ): Set[Path] =
      children.foldLeft(alive)((res, child) => child._1.filterDeadPaths(currentPath, res))
  end LayoutWidget

  final def layout[Event](
                            children : List[Widget[Event]],
                            placementStrategy : List[Widget[Event]] => PlacementEffect[List[(PlacedWidget[Event, SystemEvent], ChildrenMeta)]]
                          ) : Widget[Event] =
    Monad[PlacementEffect].map(placementStrategy(children))(placedChilren => constructRealWidget(LayoutWidget(placedChilren, layout(_, placementStrategy))))
  end layout
end LayoutLibrary
