package me.katze.gui4s.widget
package handle

import draw.Drawable
import free.AsFree
import merge.Mergable

import catnip.BiMonad
import catnip.syntax.all.given
import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.widget.CatchEvents

def statefulHandlesEvent[
  Update[_, _] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Widget,
  State : Equiv as StateEQ,
  Event,
  ChildEvent,
  HandleableEvent
](
   stateHandlesEvents  : HandlesEvent[State, NonEmptyList[ChildEvent], Update[Event, State]],
   drawStateIntoWidget: Drawable[State, Place[Widget]],
   childHandlesEvents  : HandlesEvent[Widget, HandleableEvent, Update[ChildEvent, Place[Widget]]],
   widgetsAreMergable  : Mergable[Place, Widget],
   widgetAsFree        : AsFree[Widget, Place[Widget]],
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
      (events, newChildWidget) <- childHandlesEvents(
        self.child,
        pathToParent.appendLast(self.name),
        event
      ).catchEvents[Event]
      newState <- NonEmptyList.fromList(events).map(stateHandlesEvents(self.state, pathToParent, _)).getOrElse(self.state.pure[Update[Event, *]])
      newChildFreeWidget =
        if StateEQ.equiv(self.state, newState) then
          widgetsAreMergable.merge(
            pathToParent.appendLast(self.name),
            self.child,
            newChildWidget,
          )
        else
          widgetsAreMergable.merge(
            pathToParent.appendLast(self.name),
            self.child,
            newChildWidget,
            drawStateIntoWidget(newState)
          )
    yield newChildFreeWidget.map(newChild => self.copy(state = newState, child = newChild))
end statefulHandlesEvent
