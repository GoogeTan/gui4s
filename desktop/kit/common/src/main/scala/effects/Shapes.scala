package gui4s.desktop.kit
package common.effects

import gui4s.core.geometry.Rect
import io.github.humbleui.skija.Path
import io.github.humbleui.types.{RRect as SkijaRounedRect, Rect as SkijaRect}

object Shapes:
  def round(rect : Rect[Float]) : Clip =
    new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height))
  end round

  def roundedCorners(radius : Float)(rect: Rect[Float]): Clip =
    new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius))
  end roundedCorners
end Shapes
