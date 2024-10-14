package me.katze.gui4s.widget
package impl

import stateful.{BiMonad, *}

import cats.*
import cats.syntax.all.{*, given}

final class FreeStatefulFabricImpl[
  Update[+_, +_] : BiMonad : CatchEvents,
  Merge[+_] : Monad,
  Draw : StatefulDraw,
  WidgetTask[+_],
  PlacedWidgetTree[+RaisesEvent, -HandlesEvent] <: PlacedWidget[Update, Merge, Draw, WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaisesEvent, HandlesEvent],
  PlacementEffect[+_] : Functor,
  RaiseableEvent, HandleableEvent >: TaskFinished,
  ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
](
  runMerge : [T] => Merge[T] => Update[T, Nothing],
  constructRealWidget : PlacedWidget[Update, Merge, Draw, WidgetTask[Any], [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]], RaiseableEvent, HandleableEvent] => PlacedWidgetTree[RaiseableEvent, HandleableEvent]
)(
  using 
    Mergeable[Merge, PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]],
    RichTypeChecker[ChildRaiseableEvent]
) extends FreeStatefulFabric[
  [T] =>> Update[T, RaiseableEvent],
  Merge,
  WidgetTask,
  [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]],
  RaiseableEvent, HandleableEvent,
  ChildRaiseableEvent, ChildHandleableEvent
]:
  override def apply(
                      name     : String,
                      state    : State[[W] =>> Update[W, RaiseableEvent], Merge, WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]],
                      childTree: PlacementEffect[PlacedWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]]
                    ): PlacementEffect[PlacedWidgetTree[RaiseableEvent, HandleableEvent]] =
      given this.type = this
      childTree.map(placedChildTree =>
        Stateful[
          Update,
          Merge,
          Draw,
          WidgetTask,
          [A, B] =>> PlacementEffect[PlacedWidgetTree[A, B]],
          PlacedWidgetTree,
          RaiseableEvent, HandleableEvent,
          ChildRaiseableEvent, ChildHandleableEvent
        ](name, state, placedChildTree, runMerge)
      ).map(constructRealWidget)
  end apply
end FreeStatefulFabricImpl

