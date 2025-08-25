package gui4s.core.widget
package handle

import draw.Drawable
import merge.Mergable

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Functor, Monad}

import scala.collection.immutable.List

def statefulHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Widget,
  State : Equiv as stateEquiality,
  ChildEvent,
  HandleableEvent
](
    stateHandlesEvents  : HandlesEvent[State, NonEmptyList[ChildEvent], Update[State]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childWidgetHandlesEvent  : HandlesEvent[Widget, HandleableEvent, Update[(Place[Widget], List[ChildEvent])]],
    widgetsAreMergable  : Mergable[Place[Widget]],
) : HandlesEvent[
  Stateful[Widget, State],
  HandleableEvent,
  Update[Place[Stateful[Widget, State]]]
] =
  (
    self: Stateful[Widget, State],
    pathToParent: Path,
    event: HandleableEvent
  ) =>
    for
      (newChildWidget, events) <- childWidgetHandlesEvent(
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
