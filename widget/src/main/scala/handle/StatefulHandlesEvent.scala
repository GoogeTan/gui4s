package me.katze.gui4s.widget
package handle

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.merge.Mergable
import me.katze.gui4s.widget.{CatchEvents, EventReaction}

def statefulHandlesEvent[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Widget,
  State,
  Event,
  ChildEvent,
  HandleableEvent
](
    stateHandlesEvents  : HandlesEvent[State, List[ChildEvent], Update[State, Event]],
    drawStateIntoWidget: Drawable[State, Place[Widget]],
    childHandlesEvents  : HandlesEvent[Widget, HandleableEvent, Update[Place[Widget], ChildEvent]],
    widgetsAreMergable  : Mergable[Place, Widget],
    widgetAsFree        : AsFree[Widget, Place[Widget]],
) : HandlesEvent[
  Stateful[Widget, State],
  HandleableEvent,
  Update[Place[Stateful[Widget, State]], Event]
] =
  (
    self: Stateful[Widget, State],
    pathToParent: Path,
    event: HandleableEvent
  ) =>
    for
      (newChildWidget, events) <- childHandlesEvents(
        self.child,
        pathToParent.appendLast(self.name),
        event
      ).catchEvents
      newState <- stateHandlesEvents(self.state, pathToParent, events)
      newChildFreeWidget =
        widgetsAreMergable.merge(
          self.child,
          newChildWidget,
          drawStateIntoWidget(newState)
        )
    yield newChildFreeWidget.map(newChild => self.copy(child = newChild))
end statefulHandlesEvent
