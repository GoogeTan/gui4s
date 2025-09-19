package gui4s.core.geometry

import scala.reflect.ClassTag

final case class CornerRounding[+MeasurementUnit](xRadius : MeasurementUnit, yRadius : MeasurementUnit)

object CornerRounding:
  def zero[MeasurementUnit : Numeric as N] : CornerRounding[MeasurementUnit] =
    CornerRounding(N.zero, N.zero)
  end zero

  def withRadius[MeasurementUnit](radius : MeasurementUnit) : CornerRounding[MeasurementUnit] =
    CornerRounding(radius, radius)
  end withRadius
end CornerRounding

final case class RRect[+MeasurementUnit](
                                          rect : Rect[MeasurementUnit],
                                          topLeft : CornerRounding[MeasurementUnit],
                                          topRight : CornerRounding[MeasurementUnit],
                                          bottomRight : CornerRounding[MeasurementUnit],
                                          bottomLeft : CornerRounding[MeasurementUnit]
                                        ):
  def radiiArray[T >: MeasurementUnit:  ClassTag] : Array[T] =
    Array(
      topLeft.xRadius, topRight.yRadius,
      topRight.xRadius, topRight.yRadius,
      bottomRight.xRadius, bottomRight.yRadius,
      bottomLeft.xRadius, bottomLeft.yRadius,
    )
  end radiiArray
end RRect

object RRect:
  def rect[MeasurementUnit : Numeric](rect: Rect[MeasurementUnit]) : RRect[MeasurementUnit] =
    RRect(
      rect,
      CornerRounding.zero,
      CornerRounding.zero,
      CornerRounding.zero,
      CornerRounding.zero,
    )
  end rect

  def roundedRect[MeasurementUnit : Numeric](rect: Rect[MeasurementUnit], radius : MeasurementUnit) : RRect[MeasurementUnit] =
    RRect(
      rect,
      CornerRounding.withRadius(radius),
      CornerRounding.withRadius(radius),
      CornerRounding.withRadius(radius),
      CornerRounding.withRadius(radius),
    )
  end roundedRect

  //TODO add oval and circle
end RRect
