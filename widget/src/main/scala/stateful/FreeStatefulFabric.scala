package me.katze.gui4s.widget
package stateful

@FunctionalInterface
trait FreeStatefulFabric[WidgetTask[+_], FreeWidgetTree[+_, -_], RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent]:
  def apply(
              name : String,
              state : State[WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
              childTree : FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
            ) : FreeWidgetTree[RaiseableEvent, HandleableEvent]
end FreeStatefulFabric
