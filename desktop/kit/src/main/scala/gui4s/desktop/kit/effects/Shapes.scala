package gui4s.desktop.kit
package effects

import io.github.humbleui.skija._
import io.github.humbleui.types.{RRect => SkijaRounedRect}
import io.github.humbleui.types.{Rect => SkijaRect}

import gui4s.core.geometry.Rect

object Shapes:
  def round(rect : Rect[Float]) : Clip =
    new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height))
  end round

  def roundedCorners(radius : Float)(rect: Rect[Float]): Clip =
    new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius))
  end roundedCorners
end Shapes
