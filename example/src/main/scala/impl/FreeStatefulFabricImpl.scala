package me.katze.gui4s.example
package impl

import stateful.*

import cats.*
import cats.syntax.all.{*, given}

class FreeStatefulFabricImpl[
  WidgetTask[+_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  PlacementEffect[+_] : Functor,
  RaiseableEvent, HandleableEvent >: TaskFinished,
  ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
](
  constructRealWidget : PlacedWidget[WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent] => PlacedWidgetTree[RaiseableEvent, HandleableEvent]
)(
  using
    FWT                       : Mergeable[PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]],
    ChildRaisableEventChecker : RichTypeChecker[ChildRaiseableEvent]
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
          WidgetTask,
          [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]],
          PlacedWidgetTree,
          RaiseableEvent, HandleableEvent,
          ChildRaiseableEvent, ChildHandleableEvent
        ](name, state, placedChildTree)(using FWT, this, ChildRaisableEventChecker)
      ).map(constructRealWidget)
  end apply
end FreeStatefulFabricImpl

