package gui4s.core.widget
package draw

def statefulIsDrawable[
  PlacedWidget,
  State,
  Draw
](childIsDrawable : Drawable[PlacedWidget, Draw]) : Drawable[Stateful[PlacedWidget, State], Draw] =
  self => childIsDrawable(self.child)
end statefulIsDrawable