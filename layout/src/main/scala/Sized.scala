package me.katze.gui4s.layout

final case class Sized[+MeasurementUnit, +T](value : T, width : MeasurementUnit, height : MeasurementUnit):
  def mapValue[B](f : T => B) : Sized[MeasurementUnit, B] =
    Sized(f(value), width, height)
  end mapValue

  /**
   * Возвращает ширину вдоль оси
   * @param axis
   * @return
   */
  def lengthAlong(axis : Axis) : MeasurementUnit =
    axis match
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end lengthAlong
  
  def lengthAlongAnother(axis: Axis): MeasurementUnit =
    axis match
      case Axis.Vertical => width
      case Axis.Horizontal => height
    end match
  end lengthAlongAnother
end Sized
