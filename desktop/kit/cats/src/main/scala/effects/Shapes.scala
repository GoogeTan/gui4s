package gui4s.desktop.kit.cats
package effects

import io.github.humbleui.skija.Path
import io.github.humbleui.types.{RRect as SkijaRounedRect, Rect as SkijaRect}
import gui4s.core.geometry.Rect

object Shapes:
  def round(rect : Rect[Float]) : Clip =
    new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height))
  end round

  def roundedCorners(rect: Rect[Float], radius: Float): Clip =
    new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius))
  end roundedCorners
end Shapes
