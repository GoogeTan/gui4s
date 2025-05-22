package me.katze.gui4s.widget
package refactor.handle

import refactor.free.AsFree
import refactor.{Stateful, StatefulState}
import stateful.{CatchEvents, EventReaction}

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.Functor
import cats.syntax.all.*
import me.katze.gui4s.widget.refactor.draw.Drawable
import me.katze.gui4s.widget.refactor.merge.Mergable

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
          widgetAsFree(self.child),
          newChildWidget,
          drawStateIntoWidget(newState)
        )
    yield newChildFreeWidget.map(newChild => self.copy(child = newChild))
end statefulHandlesEvent
