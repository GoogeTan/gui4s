package gui4s.core.widget
package handle

import scala.collection.immutable.List

import cats.Functor
import cats.Monad
import cats._
import cats.data.NonEmptyList
import cats.syntax.all._

import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.merge.Mergable

// TODO добавить тесты на добавление имен
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
        pathToParent / self.name,
        event
      )
      newState : Option[State] <-
        NonEmptyList.fromList(events)
          .traverse(stateHandlesEvents(self.stateBehaviour, pathToParent / self.name, _))
    yield newState
      .filterNot(stateEquiality.equiv(_, self.stateBehaviour))
      .map(newState =>
        widgetsAreMergable.merge(
          pathToParent / self.name,
          newChildWidget,
          drawStateIntoWidget(newState)
        ).map(newChild =>
          self.copy(stateBehaviour = newState, child = newChild)
        )
      )
      .getOrElse(newChildWidget.map(newChild => self.copy(child = newChild)))
end statefulHandlesEvent
