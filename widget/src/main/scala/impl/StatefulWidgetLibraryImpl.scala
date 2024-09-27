package me.katze.gui4s.widget
package impl

import library.StatefulLibrary
import placeable.Placeable
import stateful.*

import me.katze.gui4s.widget

trait StatefulWidgetLibraryImpl[F[+_], Draw, Bounds] extends WidgetLibraryImpl[F, Draw, Bounds] with StatefulLibrary:
  override def statefulFabric[RaiseableEvent, HandleableEvent >: TaskFinished, ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent](
    using RichTypeChecker[ChildRaiseableEvent]
  ): FreeStatefulFabric[WidgetTask, [A, B] =>> Placeable[Bounds, Magic[A, B]], RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent] =
      FreeStatefulFabricImpl[Draw, WidgetTask, Magic, PlacementEffect, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent](constructRealWidget[RaiseableEvent, HandleableEvent])(using statefulIsDrawable, placementIsEffect, freeTreesAreMergeable)


  override given statefulIsDrawable: StatefulDraw[Draw] = new StatefulDraw[Draw]:
    override def drawStateful[T](name     : String, state    : State[Any, T, Any, Any], childTree: widget.PlacedWidget[Draw, Any, [A, B] =>> Any, Any, Nothing]): Draw = childTree.draw
  end statefulIsDrawable
end StatefulWidgetLibraryImpl
