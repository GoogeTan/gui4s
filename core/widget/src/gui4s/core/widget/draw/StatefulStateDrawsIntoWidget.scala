package gui4s.core.widget
package draw

def statefulStateDrawsIntoWidget[
  State,
  Widget,
  EventHandler
] : Drawable[StatefulBehaviour[State, State => Widget, EventHandler], Widget] =
  self => self.draw(self.state.currentState)
end statefulStateDrawsIntoWidget
