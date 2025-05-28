package me.katze.gui4s.widget
package draw

def statefulStateDrawsIntoWidget[
  State,
  TaskSupervisor,
  Widget,
  EventHandler,
  Destructor
] : Drawable[StatefulState[State, TaskSupervisor, State => Widget, EventHandler, Destructor], Widget] =
  self => self.draw(self.currentState)
end statefulStateDrawsIntoWidget
