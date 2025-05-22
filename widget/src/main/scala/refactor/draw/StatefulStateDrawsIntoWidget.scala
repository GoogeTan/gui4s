package me.katze.gui4s.widget
package refactor.draw

import refactor.StatefulState

final class StatefulStateDrawsIntoWidget[
  State,
  TaskSupervisor,
  Widget,
  EventHandler,
  Destructor
] extends Drawable[StatefulState[State, TaskSupervisor, State => Widget, EventHandler, Destructor], Widget]:
  override def draw(self: StatefulState[State, TaskSupervisor, State => Widget, EventHandler, Destructor]): Widget =
    self.draw(self.currentState)
  end draw
end StatefulStateDrawsIntoWidget
