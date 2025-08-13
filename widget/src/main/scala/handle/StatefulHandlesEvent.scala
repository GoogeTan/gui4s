package me.katze.gui4s.widget
package handle

import draw.Drawable
import merge.Mergable

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Functor, Monad}

def statefulHandlesEvent[
  Update[_, _],
  Place[_] : Functor,
  Widget,
  State : Equiv as stateEquiality,
  Event,
  ChildEvent,
  HandleableEvent
](
    using Monad[Update[Event, *]]
)(
    stateHandlesEvents  : HandlesEvent[State, NonEmptyList[ChildEvent], Update[Event, State]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childWidgetHandlesEvent  : HandlesEvent[Widget, HandleableEvent, Update[Event, (List[ChildEvent], Place[Widget])]],
    widgetsAreMergable  : Mergable[Place[Widget]],
) : HandlesEvent[
  Stateful[Widget, State],
  HandleableEvent,
  Update[Event, Place[Stateful[Widget, State]]]
] =
  (
    self: Stateful[Widget, State],
    pathToParent: Path,
    event: HandleableEvent
  ) =>
    for
      (events, newChildWidget) <- childWidgetHandlesEvent(
        self.child,
        pathToParent.appendLast(self.name),
        event
      )
      newState : Option[State] <- NonEmptyList.fromList(events).traverse(stateHandlesEvents(self.stateBehaviour, pathToParent.appendLast(self.name), _))
    yield newState
      .filterNot(stateEquiality.equiv(_, self.stateBehaviour))
      .map(newState =>
        widgetsAreMergable.merge(
          pathToParent.appendLast(self.name),
          newChildWidget,
          drawStateIntoWidget(newState)
        ).map(newChild =>
          self.copy(stateBehaviour = newState, child = newChild)
        )
      )
      .getOrElse(newChildWidget.map(newChild => self.copy(child = newChild)))
end statefulHandlesEvent
