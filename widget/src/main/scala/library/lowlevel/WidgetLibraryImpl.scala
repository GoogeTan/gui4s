package me.katze.gui4s.widget
package library.lowlevel

import placeable.given
import stateful.{Mergeable, Path, TaskFinished}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget

final class WidgetLibraryImpl[UpdateIn[+_, +_] : Bifunctor, MergeIn[+_] : Monad, DrawIn, PlacementEffectIn[+_], WidgetTaskIn, SystemEventIn](
  using override val placementIsEffect: Monad[PlacementEffectIn]
)(
  val swapEffects : [A] => PlacementEffectIn[MergeIn[A]] => MergeIn[PlacementEffectIn[A]]
) extends WidgetLibrary:
  override type Draw = DrawIn
  override type PlacedWidget[+A, -B] = Magic[A, B]
  override type WidgetTask = WidgetTaskIn
  override type SystemEvent = SystemEventIn
  override type PlacementEffect[+E] = PlacementEffectIn[E]
  override type Update = UpdateIn
  override type Merge = MergeIn

  final class Magic[+A, -B](
                              preWidget : widget.PlacedWidget[Update, Merge, Draw, WidgetTask, [C, D] =>> PlacementEffect[Magic[C, D]], A, B]
                            ) extends widget.PlacedWidget[Update, Merge, Draw, WidgetTask, [C, D] =>> PlacementEffect[Magic[C, D]], A, B]:
    override def handleDownEvent(event: B): Update[PlacementEffect[Magic[A, B]], A] = preWidget.handleDownEvent(event)

    override def asFree: PlacementEffect[Magic[A, B]] = preWidget.asFree

    override def mergeWithState(oldState: Map[String, Any]): Merge[PlacementEffect[Magic[A, B]]] = preWidget.mergeWithState(oldState).map(_.map(Magic(_)))

    override def childrenStates: Map[String, Any] = preWidget.childrenStates

    override def draw: Draw = preWidget.draw

    override def filterDeadPaths(
                                  currentPath: Path,
                                  alive      : Set[Path]
                                ): Set[Path] = 
      preWidget.filterDeadPaths(currentPath, alive)
    end filterDeadPaths
  end Magic

  override def constructRealWidget[RaisableEvent, HandleableEvent](
                                                                    widget: me.katze.gui4s.widget.PlacedWidget[Update, Merge, Draw, WidgetTask, [C, D] =>> PlacementEffect[Magic[C, D]], RaisableEvent, HandleableEvent]
                                                                  ) : Magic[RaisableEvent, HandleableEvent] =
    widget match
      case magic: Magic[RaisableEvent, HandleableEvent] => magic
      case anythingElse => Magic(anythingElse)
    end match
  end constructRealWidget

  override def freeTreesAreMergeable[A, B]: Mergeable[Merge, PlacementEffect[Magic[A, B]]] =
    (oldOne: PlacementEffect[Magic[A, B]], newOne: PlacementEffect[Magic[A, B]]) =>
      swapEffects(
        Monad[PlacementEffect]
          .map2(oldOne, newOne)((a : Magic[A, B], b : Magic[A, B]) => b.mergeWithState(a.childrenStates))
      ).map(_.flatten)
  end freeTreesAreMergeable
end WidgetLibraryImpl
