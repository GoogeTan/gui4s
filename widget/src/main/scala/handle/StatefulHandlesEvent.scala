package me.katze.gui4s.widget
package handle

import draw.Drawable
import free.AsFree
import merge.Mergable

import catnip.BiMonad
import catnip.syntax.all.given
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.CatchEvents

def statefulHandlesEvent[
  Update[_, _] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Widget,
  State,
  Event,
  ChildEvent,
  HandleableEvent
](
    stateHandlesEvents  : HandlesEvent[State, List[ChildEvent], Update[Event, State]],
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
      newState <- stateHandlesEvents(self.state, pathToParent, events)
      newChildFreeWidget =
        widgetsAreMergable.merge(
          pathToParent.appendLast(self.name),
          self.child,
          newChildWidget,
          drawStateIntoWidget(newState)
        )
    yield newChildFreeWidget.map(newChild => self.copy(child = newChild))
end statefulHandlesEvent
