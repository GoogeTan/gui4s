package me.katze.gui4s.widget
package refactor.draw

final class ConstDraw[T, Draw](draw : Draw) extends Drawable[T, Draw]:
  override def draw(self: T): Draw =
    draw
  end draw
end ConstDraw
