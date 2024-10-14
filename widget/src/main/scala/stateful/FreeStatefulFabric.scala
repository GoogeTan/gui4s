package me.katze.gui4s.widget
package stateful

@FunctionalInterface
trait FreeStatefulFabric[-Update[+_], -Merge[+_],WidgetTask[+_], FreeWidgetTree[+_, -_], RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent]:
  def apply(
              name : String,
              state : State[Update, Merge, WidgetTask[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]],
              childTree : FreeWidgetTree[ChildRaiseableEvent, ChildHandleableEvent]
            ) : FreeWidgetTree[RaiseableEvent, HandleableEvent]
end FreeStatefulFabric
