package me.katze.gui4s.widget
package refactor.draw

trait Drawable[-T, +Draw]:
  def draw(self : T) : Draw
end Drawable
