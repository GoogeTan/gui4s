package gui4s.android.kit.effects

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import org.jetbrains.skia.Path
import org.jetbrains.skia.{PathDirection, RRect as SkijaRounedRect, Rect as SkijaRect}

type ShapeInBounds = Rect[Float] => PlacementEffect[Situated[Clip]]
type Shape = Rect[Float] => Situated[Clip]

object Shape:
  val round : Shape = (rect : Rect[Float]) =>
    Sized(
      new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height), PathDirection.CLOCKWISE, 1),
      rect
    )
  end round

  def roundedCorners(radius : Float) : Shape =
    (rect: Rect[Float]) =>
      Sized(
        new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius), PathDirection.CLOCKWISE, 1),
        rect
      )
  end roundedCorners

  def rect(size : Rect[Float]) : Shape =
    rect =>
      Sized(
        new Path().addRect(SkijaRect.makeLTRB(0, 0, rect.width, rect.height), PathDirection.CLOCKWISE, 1),
        rect
      )
end Shape