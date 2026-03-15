package gui4s.desktop.skija

import io.github.humbleui.skija._

final case class SkPaint(
  color: Int = 0xFF000000,
  style: PaintMode = PaintMode.FILL,
  isAntiAlias: Boolean = false,
  isDither: Boolean = false,
  stroke: StrokeOptions = StrokeOptions(),
  shader: Option[Shader] = None,
  pathEffect: Option[PathEffect] = None,
  maskFilter: Option[MaskFilter] = None,
  colorFilter: Option[ColorFilter] = None,
  imageFilter: Option[ImageFilter] = None,
  blendMode: BlendMode = BlendMode.SRC_OVER
):
  def toSkia : Paint =
    val res = stroke.applyToSkia(
      Paint()
        .setColor(color)
        .setMode(style)
        //.setAntiAlias(isAntiAlias) this breaks skia shaders.
        .setDither(isDither)
    )
    shader.foreach(res.setShader)
    pathEffect.foreach(res.setPathEffect)
    maskFilter.foreach(res.setMaskFilter)
    colorFilter.foreach(res.setColorFilter)
    imageFilter.foreach(res.setImageFilter)
    res.setBlendMode(blendMode)
    res
  end toSkia
end SkPaint
