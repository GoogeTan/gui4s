package me.katze.gui4s.widget
package impl

import stateful.*

import cats.*
import cats.syntax.all.{*, given}

final class FreeStatefulFabricImpl[
  Draw : StatefulDraw,
  WidgetTask[+_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Draw, WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  PlacementEffect[+_] : Functor,
  RaiseableEvent, HandleableEvent >: TaskFinished,
  ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
](
  constructRealWidget : PlacedWidget[Draw, WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent] => PlacedWidgetTree[RaiseableEvent, HandleableEvent]
)(
  using Mergeable[PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]], RichTypeChecker[ChildRaiseableEvent]
) extends FreeStatefulFabric[
  WidgetTask,
  [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]],
  RaiseableEvent, HandleableEvent,
  ChildRaiseableEvent, ChildHandleableEvent
]:
  override def apply(
                      name     : String,
                      state    : State[WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]],
                      childTree: PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]
                    ): PlacementEffect[PlacedWidgetTree[RaiseableEvent, HandleableEvent]] =
      childTree.map(placedChildTree =>
        Stateful[
          Draw,
          WidgetTask,
          [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]],
          PlacedWidgetTree,
          RaiseableEvent, HandleableEvent,
          ChildRaiseableEvent, ChildHandleableEvent
        ](name, state, placedChildTree)(using summon, summon, this, summon)
      ).map(constructRealWidget)
  end apply
end FreeStatefulFabricImpl

