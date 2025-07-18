package me.katze.gui4s.widget
package draw

def statefulStateDrawsIntoWidget[
  State,
  Widget,
  EventHandler,
  Destructor
] : Drawable[StatefulBehaviour[State, State => Widget, EventHandler, Destructor], Widget] =
  self => self.draw(self.state.currentState)
end statefulStateDrawsIntoWidget
