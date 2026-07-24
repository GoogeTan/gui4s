package gui4s.core.widget
package handle

import cats.*
import cats.syntax.all.*

import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.merge.UpdateWidgetStateFromTheOldOne

// TODO добавить тесты на добавление имен
def statefulHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor as PM,
  Widget,
  State,
  ChildEvent
](
    stateHandlesEvents  : HandlesEvent[State, ChildEvent, Update[Option[State]]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childWidgetHandlesEvent  : HandlesEvent_[Widget, Update[(ChildEvent, Option[Place[Widget]])]],
    widgetsAreMergable  : UpdateWidgetStateFromTheOldOne[Place, Widget],
) : HandlesEvent_[
  Stateful[Widget, State],
  Update[Option[Place[Stateful[Widget, State]]]]
] =
  (
    self: Stateful[Widget, State]
  ) =>
    for
      (events, newChildWidget) <- childWidgetHandlesEvent(self.child)
      newState : Option[State] <- stateHandlesEvents(self.stateBehaviour, events)
    yield (newState, newChildWidget) match
      case (Some(newState), Some(newChildWidget)) =>
        PM.map(
          widgetsAreMergable.mergeUpdatedAndRerenderedWidgets(
            newChildWidget,
            drawStateIntoWidget(newState)
          )
        )(
          newChild =>
          self.copy(stateBehaviour = newState, child = newChild)
        ).some
      case (None, Some(newChildWidget)) =>
        newChildWidget.map(newChildWidgetPlaced => self.copy(child = newChildWidgetPlaced)).some
      case (Some(newState), None) =>
        PM.map(
          widgetsAreMergable.mergeOldAndRerenderedWidgets(
            self.child,
            drawStateIntoWidget(newState)
          )
        )(
          newChild =>
            self.copy(stateBehaviour = newState, child = newChild)
        ).some
      case (None, None) =>
        None
end statefulHandlesEvent
