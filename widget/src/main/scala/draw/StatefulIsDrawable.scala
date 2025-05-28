package me.katze.gui4s.widget
package draw

def statefulIsDrawable[
  PlacedWidget,
  State,
  Draw
](childIsDrawable : Drawable[PlacedWidget, Draw]) : Drawable[Stateful[PlacedWidget, State], Draw] =
  self => childIsDrawable(self.child)
end statefulIsDrawable