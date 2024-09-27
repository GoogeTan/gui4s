package me.katze.gui4s.widget
package impl

import library.{StatefulLibrary, WidgetLibrary}
import placeable.{*, given}
import stateful.{Mergeable, State, StatefulDraw, TaskFinished}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

class WidgetLibraryImpl[F[+_] : Monad, DrawIn, Bounds] extends WidgetLibrary:
  final override type Draw = DrawIn
  final override type PlacedWidget[+A, -B] = Magic[A, B]
  final override type WidgetTask[+T] = WidgetTaskImpl[F, T]
  final override type SystemEvent = TaskFinished
  final override type PlacementEffect[+E] = Placeable[Bounds, E]

  final override def placementIsEffect: Monad[PlacementEffect] = me.katze.gui4s.widget.placeable.placementIsEffect

  final class Magic[+A, -B](preWidget : widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], A, B]) extends
          me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], A, B]:
    override def handleDownEvent(event: B): EventResult[WidgetTask[Any], PlacementEffect[Magic[A, B]], A] = preWidget.handleDownEvent(event)

    override def asFree: PlacementEffect[Magic[A, B]] = preWidget.asFree

    override def mergeWithState(oldState: Map[String, Any]): Placeable[Bounds, Magic[A, B]] = preWidget.mergeWithState(oldState).map(Magic(_))

    override def childrenStates: Map[String, Any] = preWidget.childrenStates

    override def draw: Draw = preWidget.draw
  end Magic

  final override def constructRealWidget[RaisableEvent, HandleableEvent](
                                                                          widget: me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], [C, D] =>> PlacementEffect[Magic[C, D]], RaisableEvent, HandleableEvent]
                                                                        ) : Magic[RaisableEvent, HandleableEvent] =
    widget match
      case magic: Magic[RaisableEvent, HandleableEvent] => magic
      case anythingElse => Magic(anythingElse)
    end match
  end constructRealWidget

  final override def freeTreesAreMergeable[A, B]: Mergeable[Placeable[Bounds, Magic[A, B]]] =
    // Тут не лямбда, потому что их не умеет дебагер распознавать. Он просто не может зайти внутри них в данном контексте. Не известно почему. Просто так. Поэтому так.
    new Mergeable[Placeable[Bounds, Magic[A, B]]]:
      override def merge(
                          oldOne: Placeable[Bounds, Magic[A, B]],
                          newOne: Placeable[Bounds, Magic[A, B]]
                        ): Placeable[Bounds, Magic[A, B]] =
        for
          realA <- oldOne
          realB <- newOne
          result <- realB.mergeWithState(realA.childrenStates)
        yield result
      end merge
  end freeTreesAreMergeable
end WidgetLibraryImpl