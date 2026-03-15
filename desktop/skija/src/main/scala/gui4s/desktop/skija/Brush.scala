package gui4s.desktop.skija

import io.github.humbleui.skija._

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect

@FunctionalInterface
trait Brush:
  def apply(size : Rect[Float], paint : SkPaint, alpha : Float = 1.0f) : SkPaint
end Brush

object Brush:
  def solid(color : Int) : Brush =
    (_, paint, alpha) =>
      paint.copy(color = Color.withA(color, (Color.getA(color) * alpha).toInt))
  end solid

  def shader(
    shader : Shader
  ) : Brush =
    (size, paint, alpha) =>
      paint.copy(shader = Some(shader))
  end shader

  def linearGradient(
    start: Point2d[Float],
    end: Point2d[Float],
    colors: Seq[Int],
  ) : Brush =
    shader(
      Shader.makeLinearGradient(
        start.x,
        start.y,
        end.x,
        end.y,
        colors.toArray,
      )
    )

  //TODO Add more brushes
end Brush
