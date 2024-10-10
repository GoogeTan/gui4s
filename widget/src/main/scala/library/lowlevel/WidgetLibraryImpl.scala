package me.katze.gui4s.widget
package library.lowlevel

import placeable.given
import stateful.{Mergeable, Path, TaskFinished}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

class WidgetLibraryImpl[F[+_] : Monad, DrawIn, PlacementEffectIn[+_], WidgetTaskIn[+_], SystemEventIn](
  using final override val placementIsEffect: FlatMap[PlacementEffectIn]
) extends WidgetLibrary:
  final override type Draw = DrawIn
  final override type PlacedWidget[+A, -B] = Magic[A, B]
  final override type WidgetTask[+T] = WidgetTaskIn[T]
  final override type SystemEvent = SystemEventIn
  final override type PlacementEffect[+E] = PlacementEffectIn[E]

  final class Magic[+A, -B](preWidget : widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], A, B]) extends
          me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], A, B]:
    override def handleDownEvent(event: B): EventResult[WidgetTask[Any], PlacementEffect[Magic[A, B]], A] = preWidget.handleDownEvent(event)

    override def asFree: PlacementEffect[Magic[A, B]] = preWidget.asFree

    override def mergeWithState(oldState: Map[String, Any]): PlacementEffect[Magic[A, B]] = preWidget.mergeWithState(oldState).map(Magic(_))

    override def childrenStates: Map[String, Any] = preWidget.childrenStates

    override def draw: Draw = preWidget.draw

    override def filterDeadPaths(
                                  currentPath: Path,
                                  alive      : Set[Path]
                                ): Set[Path] = 
      preWidget.filterDeadPaths(currentPath, alive)
    end filterDeadPaths
  end Magic

  final override def constructRealWidget[RaisableEvent, HandleableEvent](
                                                                          widget: me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], RaisableEvent, HandleableEvent]
                                                                        ) : Magic[RaisableEvent, HandleableEvent] =
    widget match
      case magic: Magic[RaisableEvent, HandleableEvent] => magic
      case anythingElse => Magic(anythingElse)
    end match
  end constructRealWidget

  final override def freeTreesAreMergeable[A, B]: Mergeable[PlacementEffect[Magic[A, B]]] =
    (oldOne: PlacementEffect[Magic[A, B]], newOne: PlacementEffect[Magic[A, B]]) =>
      FlatMap[PlacementEffect].flatMap2(oldOne, newOne)((a : Magic[A, B], b : Magic[A, B]) => b.mergeWithState(a.childrenStates))
  end freeTreesAreMergeable
end WidgetLibraryImpl
