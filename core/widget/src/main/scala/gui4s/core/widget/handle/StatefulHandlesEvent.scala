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
  ChildEvent
](
    stateHandlesEvents  : HandlesEvent[State, List[ChildEvent], Update[Option[State]]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childWidgetHandlesEvent  : HandlesEvent_[Widget, Update[(Option[Place[Widget]], List[ChildEvent])]],
    widgetsAreMergable  : UpdateWidgetStateFromTheOldOne[Place, Widget],
    addNameToPlacePath : String => Place ~> Place,
    addNameToUpdatePath : String => Update ~> Update,
) : HandlesEvent_[
  Stateful[Widget, State],
  Update[Option[Place[Stateful[Widget, State]]]]
] =
  (
    self: Stateful[Widget, State],
    pathToParent: Path
  ) =>
    for
      (newChildWidget, events) <- addNameToUpdatePath(self.name)(
        childWidgetHandlesEvent(
          self.child,
          pathToParent / self.name
        )
      )
      newState : Option[State] <- addNameToUpdatePath(self.name)(stateHandlesEvents(self.stateBehaviour, pathToParent / self.name, events))
    yield (newState, newChildWidget) match
      case (Some(newState), Some(newChildWidget)) =>
        addNameToPlacePath(self.name)(
          widgetsAreMergable.mergeUpdatedAndRerenderedWidgets(
            pathToParent / self.name,
            newChildWidget,
            drawStateIntoWidget(newState)
          ).map(newChild =>
            self.copy(stateBehaviour = newState, child = newChild)
          )
        ).some
      case (None, Some(newChildWidget)) =>
        newChildWidget.map(newChildWidgetPlaced => self.copy(child = newChildWidgetPlaced)).some
        addNameToPlacePath(self.name)(
          newChildWidget.map(newChildWidgetPlaced => self.copy(child = newChildWidgetPlaced))
        ).some
      case (Some(newState), None) =>
        addNameToPlacePath(self.name)(
          widgetsAreMergable.mergeOldAndRerenderedWidgets(
            pathToParent / self.name,
            self.child,
            drawStateIntoWidget(newState)
          ).map(newChild =>
            self.copy(stateBehaviour = newState, child = newChild)
          )
        ).some
      case (None, None) =>
        None
end statefulHandlesEvent
