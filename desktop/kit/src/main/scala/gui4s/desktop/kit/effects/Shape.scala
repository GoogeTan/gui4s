package gui4s.desktop.kit
package effects

import io.github.humbleui.skija.*
import io.github.humbleui.types.RRect as SkijaRounedRect
import io.github.humbleui.types.Rect as SkijaRect
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import cats.syntax.all.*

type ShapeInBounds = Rect[Float] => PlacementEffect[Situated[Clip]]
type Shape = Rect[Float] => Situated[Clip]

extension(shape : Shape)
  def withBounds : ShapeInBounds =
    rect => shape(rect).pure
  end withBounds
end extension

object Shape:
  val round : Shape = (rect : Rect[Float]) =>
    Sized(Path.makeOval(SkijaRect(0f, 0f, rect.width, rect.height)), rect)
  end round

  def roundedCorners(radius : Float) : Shape =
    (rect: Rect[Float]) =>
      Sized(Path.makeRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius)), rect)
  end roundedCorners

  def rect(size : Rect[Float]) : Shape =
    rect =>
      Sized(Path.makeRect(SkijaRect.makeLTRB(0f, 0f, size.width, size.height)), rect)
end Shape