package me.katze.gui4s.widget
package draw

def statefulStateDrawsIntoWidget[
  State,
  Widget,
  EventHandler,
  Destructor
] : Drawable[StatefulState[State, State => Widget, EventHandler, Destructor], Widget] =
  self => self.draw(self.currentState)
end statefulStateDrawsIntoWidget
