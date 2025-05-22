package me.katze.gui4s.widget
package refactor.draw

import refactor.Stateful

final class StatefulIsDrawable[
  PlacedWidget,
  State,
  Draw
](using childIsDrawable : Drawable[PlacedWidget, Draw]) extends Drawable[Stateful[PlacedWidget, State], Draw]:
  override def draw(self: Stateful[PlacedWidget, State]): Draw =
    childIsDrawable.draw(self.child)
  end draw
end StatefulIsDrawable