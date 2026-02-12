package gui4s.desktop.kit
package effects

import io.github.humbleui.skija._
import io.github.humbleui.types.{RRect => SkijaRounedRect}
import io.github.humbleui.types.{Rect => SkijaRect}

import gui4s.core.geometry.Rect

object Shapes:
  def round(rect : Rect[Float]) : Clip =
    Path.makeOval(SkijaRect(0f, 0f, rect.width, rect.height))
  end round

  def roundedCorners(radius : Float)(rect: Rect[Float]): Clip =
    Path.makeRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius))
  end roundedCorners
end Shapes
