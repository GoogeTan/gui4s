package gui4s.core.widget
package handle

import scala.collection.immutable.List

import cats.Functor
import cats.Monad
import cats._
import cats.data.NonEmptyList
import cats.syntax.all._

import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.merge.UpdateWidgetStateFromTheOldOne

// TODO добавить тесты на добавление имен
def statefulHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Widget,
  State,
  ChildEvent,
  EnvironmentalEvent
](
    stateHandlesEvents  : HandlesEvent[State, NonEmptyList[ChildEvent], Update[Option[State]]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childWidgetHandlesEvent  : HandlesEvent[Widget, EnvironmentalEvent, Update[(Option[Place[Widget]], List[ChildEvent])]],
    widgetsAreMergable  : UpdateWidgetStateFromTheOldOne[Place, Widget],
) : HandlesEvent[
  Stateful[Widget, State],
  EnvironmentalEvent,
  Update[Option[Place[Stateful[Widget, State]]]]
] =
  (
    self: Stateful[Widget, State],
    pathToParent: Path,
    event: EnvironmentalEvent
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
          .map(_.flatten)
    yield (newState, newChildWidget) match
      case (Some(newState), Some(newChildWidget)) =>
        widgetsAreMergable.mergeUpdatedAndRerenderedWidgets(
          pathToParent / self.name,
          newChildWidget,
          drawStateIntoWidget(newState)
        ).map(newChild =>
          self.copy(stateBehaviour = newState, child = newChild)
        ).some
      case (None, Some(newChildWidget)) =>
        newChildWidget.map(newChildWidgetPlaced => self.copy(child = newChildWidgetPlaced)).some
      case (Some(newState), None) =>
        widgetsAreMergable.mergeOldAndRerenderedWidgets(
          pathToParent / self.name,
          self.child,
          drawStateIntoWidget(newState)
        ).map(newChild =>
          self.copy(stateBehaviour = newState, child = newChild)
        ).some
      case (None, None) =>
        None
end statefulHandlesEvent
