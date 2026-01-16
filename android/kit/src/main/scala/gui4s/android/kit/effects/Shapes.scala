package gui4s.android.kit.effects

import gui4s.core.geometry.Rect

import org.jetbrains.skia.Path
import org.jetbrains.skia.{PathDirection, RRect as SkijaRounedRect, Rect as SkijaRect}

object Shapes:
  def round(rect : Rect[Float]) : Clip =
    new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height), PathDirection.CLOCKWISE, 1)
  end round

  def roundedCorners(radius : Float)(rect: Rect[Float]): Clip =
    new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius), PathDirection.CLOCKWISE, 1)
  end roundedCorners
end Shapes
