package me.katze.gui4s.example
package impl

import placeable.Placeable
import stateful.{Mergeable, TaskFinished}

import cats.Monad
import cats.effect.IO
import me.katze.gui4s.example
import me.katze.gui4s.example.library.StatefulFabric

object StatefulFabricImpl extends StatefulFabric:
  override type PlacedWidget[+A, -B] = Magic[A, B]
  override type WidgetTask[+T] = WidgetTaskImpl[IO, T]
  override type SystemEvent = TaskFinished
  override type PlacementEffect[+E] = Placeable[E]

  override def placementIsEffect: Monad[Placeable] = me.katze.gui4s.example.placeable.placementIsEffect

  class Magic[+A, -B](preWidget : example.PlacedWidget[WidgetTask[Any], FreeT[Magic], A, B]) extends me.katze.gui4s.example.PlacedWidget[WidgetTask[Any], FreeT[Magic], A, B]:
    override def handleDownEvent(event: B): EventResult[WidgetTask[Any], Placeable[Magic[A, B]], A] = preWidget.handleDownEvent(event)

    override def asFree: FreeT[Magic][A, B] = preWidget.asFree

    override def mergeWithState(oldState: Map[String, Any]): Placeable[Magic[A, B]] = preWidget.mergeWithState(oldState).map(Magic(_))

    override def childrenStates: Map[String, Any] = preWidget.childrenStates

    override def prettyString: String = preWidget.prettyString
  end Magic

  override def constructRealWidget[Event](widget : example.PlacedWidget[WidgetTask[Any], FreeT[Magic], Event, TaskFinished]) : Magic[Event, TaskFinished] =
    widget match
      case magic: Magic[Event, TaskFinished] => magic
      case anythingElse => Magic(anythingElse)
    end match
  end constructRealWidget

  override def freeTreesAreMergeable[Event]: Mergeable[Placeable[Magic[Event, TaskFinished]]] =
    // Тут не лямбда, потому что их не умеет дебагер распознавать. Он просто не может зайти внутри них в данном контексте. Не известно почему. Просто так. Поэтому так.
    new Mergeable[Placeable[Magic[Event, TaskFinished]]]:
      override def merge(
                          oldOne: Placeable[Magic[Event, TaskFinished]],
                          newOne: Placeable[Magic[Event, TaskFinished]]
                        ): Placeable[Magic[Event, TaskFinished]] =
        for
          realA <- oldOne
          realB <- newOne
          result <- realB.mergeWithState(realA.childrenStates)
        yield result
end StatefulFabricImpl